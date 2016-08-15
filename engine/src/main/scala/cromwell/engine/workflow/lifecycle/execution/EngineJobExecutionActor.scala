package cromwell.engine.workflow.lifecycle.execution

import akka.actor.{ActorRef, LoggingFSM, Props}
import cromwell.backend.BackendJobExecutionActor._
import cromwell.backend.{BackendInitializationData, BackendJobDescriptor, BackendJobDescriptorKey, BackendLifecycleActorFactory}
import cromwell.core.logging.WorkflowLogging
import cromwell.core.Dispatcher.EngineDispatcher
import cromwell.core._
import cromwell.database.CromwellDatabase
import cromwell.database.sql.MetaInfoId
import cromwell.engine.workflow.lifecycle.execution.EngineJobExecutionActor._
import cromwell.engine.workflow.lifecycle.execution.JobPreparationActor.{BackendJobPreparationFailed, BackendJobPreparationSucceeded}
import cromwell.engine.workflow.lifecycle.execution.callcaching.EngineJobHashingActor.{CacheHit, CacheMiss, CallCacheHashes, HashError}
import cromwell.engine.workflow.lifecycle.execution.callcaching._
import cromwell.jobstore.JobStoreActor._
import cromwell.jobstore.{Pending => _, _}
import cromwell.core.ExecutionIndex.IndexEnhancedIndex
import scala.util.{Failure, Success, Try}

class EngineJobExecutionActor(jobKey: BackendJobDescriptorKey,
                              executionData: WorkflowExecutionActorData,
                              factory: BackendLifecycleActorFactory,
                              initializationData: Option[BackendInitializationData],
                              restarting: Boolean,
                              serviceRegistryActor: ActorRef,
                              jobStoreActor: ActorRef,
                              backendName: String,
                              callCachingMode: CallCachingMode) extends LoggingFSM[EngineJobExecutionActorState, EJEAData] with WorkflowLogging {

  override val workflowId = executionData.workflowDescriptor.id

  val jobTag = s"${workflowId.shortString}:${jobKey.call.fullyQualifiedName}:${jobKey.index.fromIndex}:${jobKey.attempt}"
  val tag = s"EJEA_$jobTag"

  // There's no need to check for a cache hit again if we got preempted
  // NB: this can also change (e.g. if we have a HashError we just force this to CallCachingOff)
  private var effectiveCallCachingMode = if (jobKey.attempt > 1) { callCachingMode.withoutRead } else { callCachingMode }

  val effectiveCallCachingKey = "Effective call caching mode"

  log.info(s"$tag: $effectiveCallCachingKey: ${effectiveCallCachingMode.getClass.getSimpleName}")
  writeToMetadata(Map(effectiveCallCachingKey -> effectiveCallCachingMode.getClass.getSimpleName))

  startWith(Pending, NoData)

  // When Pending, the FSM always has NoData
  when(Pending) {
    case Event(Execute, NoData) =>
      if (restarting) {
        val jobStoreKey = jobKey.toJobStoreKey(workflowId)
        jobStoreActor ! QueryJobCompletion(jobStoreKey)
        goto(CheckingJobStore)
      } else {
        prepareJob()
      }
  }

  // When CheckingJobStore, the FSM always has NoData
  when(CheckingJobStore) {
    case Event(JobNotComplete, NoData) =>
      prepareJob()
    case Event(JobComplete(jobResult), NoData) =>
      jobResult match {
        case JobResultSuccess(returnCode, jobOutputs) =>
          context.parent ! SucceededResponse(jobKey, returnCode, jobOutputs)
          context stop self
          stay()
        case JobResultFailure(returnCode, reason, false) =>
          context.parent ! FailedNonRetryableResponse(jobKey, reason, returnCode)
          context stop self
          stay()
        case JobResultFailure(returnCode, reason, true) =>
          context.parent ! FailedRetryableResponse(jobKey, reason, returnCode)
          context stop self
          stay()
      }
    case Event(f: JobStoreReadFailure, NoData) =>
      log.error(f.reason, "{}: Error reading from JobStore", tag)
      // Escalate
      throw new RuntimeException(f.reason)
  }

  // When PreparingJob, the FSM always has NoData
  when(PreparingJob) {
    case Event(BackendJobPreparationSucceeded(jobDescriptor, bjeaProps), NoData) =>
      effectiveCallCachingMode match {
        case activity: CallCachingActivity if activity.readFromCache =>
          initializeJobHashing(jobDescriptor, activity)
          goto(CheckingCallCache) using EJEAJobDescriptorData(Option(jobDescriptor), Option(bjeaProps))
        case activity: CallCachingActivity =>
          initializeJobHashing(jobDescriptor, activity)
          runJob(jobDescriptor, bjeaProps)
        case CallCachingOff => runJob(jobDescriptor, bjeaProps)
      }
    case Event(response: BackendJobPreparationFailed, NoData) =>
      context.parent forward response
      context stop self
      stay()
  }

  // When CheckingCallCache, the FSM always has EJEAJobDescriptorData
  private val callCachingReadResultMetadataKey = "Call caching read result"
  when(CheckingCallCache) {
    case Event(HashError(t), EJEAJobDescriptorData(Some(jobDescriptor), Some(bjeaProps))) =>
      writeToMetadata(Map(callCachingReadResultMetadataKey -> s"Hashing Error: ${t.getMessage}"))
      recordHashError(t)
      runJob(jobDescriptor, bjeaProps)
    case Event(CacheMiss, EJEAJobDescriptorData(Some(jobDescriptor), Some(bjeaProps))) =>
      writeToMetadata(Map(callCachingReadResultMetadataKey -> "Cache Miss"))
      log.info(s"Cache miss for job ${jobDescriptor.key.call.fullyQualifiedName}, index ${jobDescriptor.key.index}")
      runJob(jobDescriptor, bjeaProps)
    case Event(CacheHit(cacheResultId), EJEAJobDescriptorData(Some(jobDescriptor), _)) =>
      writeToMetadata(Map(callCachingReadResultMetadataKey -> s"Cache Hit (from result ID $cacheResultId)"))
      log.info(s"Cache hit for job ${jobDescriptor.key.call.fullyQualifiedName}, index ${jobDescriptor.key.index}! Copying cache result $cacheResultId")
      lookupCachedResult(jobDescriptor, cacheResultId)
  }

  // When RunningJob, the FSM always has EJEAPartialCompletionData (which might be None, None)
  when(RunningJob) {
    case Event(response: SucceededResponse, EJEAPartialCompletionData(None, Some(Success(hashes)))) if effectiveCallCachingMode.writeToCache =>
      saveCacheResults(EJEASuccessfulCompletionDataWithHashes(response, hashes))
    case Event(response: SucceededResponse, data @ EJEAPartialCompletionData(None, None)) if effectiveCallCachingMode.writeToCache =>
      stay using data.copy(jobResult = Option(response))
    case Event(hashes: CallCacheHashes, data @ EJEAPartialCompletionData(Some(response: SucceededResponse), None)) =>
      saveCacheResults(EJEASuccessfulCompletionDataWithHashes(response, hashes))
    case Event(hashes: CallCacheHashes, data @ EJEAPartialCompletionData(None, None)) =>
      stay using data.copy(hashes = Option(Success(hashes)))
    case Event(HashError(t), data @ EJEAPartialCompletionData(Some(response: BackendJobExecutionResponse), _)) =>
      recordHashError(t)
      saveJobCompletionToJobStore(response)
    case Event(HashError(t), data @ EJEAPartialCompletionData(None, _)) =>
      recordHashError(t)
      stay using data.copy(hashes = Option(Failure(t)))
    case Event(response: BackendJobExecutionResponse, data: EJEAPartialCompletionData) =>
      saveJobCompletionToJobStore(response)
  }

  // When WritingToCallCache, the FSM always has EJEASuccessfulCompletionDataWithHashes
  when(UpdatingCallCache) {
    case Event(CallCacheWriteSuccess, data @ EJEASuccessfulCompletionDataWithHashes(response, _)) =>
      saveJobCompletionToJobStore(response)
    case Event(CallCacheWriteFailure(reason), data @ EJEASuccessfulCompletionDataWithHashes(response, _)) =>
      context.parent ! FailedNonRetryableResponse(jobKey, reason, response.returnCode)
      context stop self
      stay()
  }

  // When UpdatingJobStore, the FSM always has EJEACompletionData
  when(UpdatingJobStore) {
    case Event(JobStoreWriteSuccess(_), EJEACompletionData(response)) =>
      context.parent forward response
      context stop self
      stay()
    case Event(JobStoreWriteFailure(t), EJEACompletionData(_)) =>
      context.parent ! FailedNonRetryableResponse(jobKey, new Exception(s"JobStore write failure: ${t.getMessage}", t), None)
      context.stop(self)
      stay()
  }

  onTransition {
    case fromState -> toState =>
      log.debug("Transitioning from {}({}) to {}({})", fromState, stateData, toState, nextStateData)
  }

  whenUnhandled {
    case Event(msg, _) =>
      log.error(s"Bad message to EngineJobExecutionActor in state $stateName(with data $stateData): $msg")
      stay
  }

  private def recordHashError(reason: Throwable) = {
    log.error("{}: hash error: {}. Disabling call caching for this job", tag, reason.getMessage)
    effectiveCallCachingMode = CallCachingOff
  }

  def prepareJob() = {
    val jobPreparationActorName = s"BackendPreparationActor_for_$jobTag"
    val jobPrepProps = JobPreparationActor.props(executionData, jobKey, factory, initializationData, serviceRegistryActor)
    val jobPreparationActor = context.actorOf(jobPrepProps, jobPreparationActorName)
    jobPreparationActor ! JobPreparationActor.Start
    goto(PreparingJob)
  }

  def initializeJobHashing(jobDescriptor: BackendJobDescriptor, activity: CallCachingActivity) = {
    val fileHasherActor = context.actorOf(BackendSpecificHasherActor.props(activity),  s"FileHasherActor_for_$jobTag")
    context.actorOf(EngineJobHashingActor.props(jobDescriptor, fileHasherActor, backendName, activity))
  }

  def lookupCachedResult(jobDescriptor: BackendJobDescriptor, cacheResultId: MetaInfoId) = {
    // TODO: Start up a backend job copying actor (if possible, otherwise just runJob). That should send back the BackendJobExecutionResponse
    self ! FailedNonRetryableResponse(jobKey, new Exception("Call cache result copying not implemented!"), None)
    // While the cache result is looked up, we wait for the response just like we were waiting for a Job to complete:
    goto(RunningJob) using EJEAPartialCompletionData(None, None)
  }

  def runJob(jobDescriptor: BackendJobDescriptor, bjeaProps: Props) = {
    val backendJobExecutionActor = context.actorOf(bjeaProps, buildJobExecutionActorName(jobDescriptor))
    val message = if (restarting) RecoverJobCommand else ExecuteJobCommand
    backendJobExecutionActor ! message
    context.parent ! JobRunning(jobDescriptor, backendJobExecutionActor)
    goto(RunningJob) using EJEAPartialCompletionData(None, None)
  }

  private def buildJobExecutionActorName(jobDescriptor: BackendJobDescriptor) = {
    s"$workflowId-BackendJobExecutionActor-${jobDescriptor.key.tag}"
  }

  private def saveCacheResults(completionData: EJEASuccessfulCompletionDataWithHashes) = {
    val callCache = new CallCache(CromwellDatabase.databaseInterface)
    context.actorOf(CallCacheWriteActor.props(callCache, workflowId, completionData.hashes, completionData.jobResult), s"CallCacheWriteActor-$tag")
    goto(UpdatingCallCache) using completionData
  }

  private def saveJobCompletionToJobStore(response: BackendJobExecutionResponse) = {
    response match {
      case SucceededResponse(jobKey: BackendJobDescriptorKey, returnCode: Option[Int], jobOutputs: JobOutputs) => saveSuccessfulJobResults(jobKey, returnCode, jobOutputs)
      case AbortedResponse(jobKey: BackendJobDescriptorKey) => log.debug("Won't save 'aborted' job response to JobStore")
      case FailedNonRetryableResponse(jobKey: BackendJobDescriptorKey, throwable: Throwable, returnCode: Option[Int]) => saveUnsuccessfulJobResults(jobKey, returnCode, throwable, retryable = false)
      case FailedRetryableResponse(jobKey: BackendJobDescriptorKey, throwable: Throwable, returnCode: Option[Int]) => saveUnsuccessfulJobResults(jobKey, returnCode, throwable, retryable = true)
    }
    goto(UpdatingJobStore) using EJEACompletionData(response)
  }

  private def saveSuccessfulJobResults(jobKey: JobKey, returnCode: Option[Int], outputs: JobOutputs) = {
    val jobStoreKey = jobKey.toJobStoreKey(workflowId)
    val jobStoreResult = JobResultSuccess(returnCode, outputs)
    jobStoreActor ! RegisterJobCompleted(jobStoreKey, jobStoreResult)
  }

  private def saveUnsuccessfulJobResults(jobKey: JobKey, returnCode: Option[Int], reason: Throwable, retryable: Boolean) = {
    val jobStoreKey = jobKey.toJobStoreKey(workflowId)
    val jobStoreResult = JobResultFailure(returnCode, reason, retryable)
    jobStoreActor ! RegisterJobCompleted(jobStoreKey, jobStoreResult)
  }

  private def writeToMetadata(keyValues: Map[String, String]) = {
    import cromwell.services.metadata.MetadataService.implicits.MetadataAutoputter
    serviceRegistryActor.putMetadata(workflowId, Option(jobKey), keyValues)
  }
}

object EngineJobExecutionActor {
  /** States */
  sealed trait EngineJobExecutionActorState
  case object Pending extends EngineJobExecutionActorState
  case object CheckingJobStore extends EngineJobExecutionActorState
  case object CheckingCallCache extends EngineJobExecutionActorState
  case object PreparingJob extends EngineJobExecutionActorState
  case object RunningJob extends EngineJobExecutionActorState
  case object UpdatingCallCache extends EngineJobExecutionActorState
  case object UpdatingJobStore extends EngineJobExecutionActorState

  /** Commands */
  sealed trait EngineJobExecutionActorCommand
  case object Execute extends EngineJobExecutionActorCommand

  final case class JobRunning(jobDescriptor: BackendJobDescriptor, backendJobExecutionActor: ActorRef)

  def props(jobDescriptorKey: BackendJobDescriptorKey,
            executionData: WorkflowExecutionActorData,
            factory: BackendLifecycleActorFactory,
            initializationData: Option[BackendInitializationData],
            restarting: Boolean,
            serviceRegistryActor: ActorRef,
            jobStoreActor: ActorRef,
            backendName: String,
            callCachingMode: CallCachingMode) = {
    Props(new EngineJobExecutionActor(jobDescriptorKey,
      executionData,
      factory,
      initializationData,
      restarting,
      serviceRegistryActor,
      jobStoreActor,
      backendName: String,
      callCachingMode)).withDispatcher(EngineDispatcher)
  }
}

sealed trait EJEAData
case object NoData extends EJEAData
case class EJEAJobDescriptorData(jobDescriptor: Option[BackendJobDescriptor], bjeaActorProps: Option[Props]) extends EJEAData {
  override def toString = s"EJEAJobDescriptorData(backendJobDescriptor: ${jobDescriptor.isDefined}, bjeaActorProps: ${bjeaActorProps.isDefined})"
}

case class EJEAPartialCompletionData(jobResult: Option[BackendJobExecutionResponse], hashes: Option[Try[CallCacheHashes]]) extends EJEAData {
    override def toString = {
      val hashesString = hashes.map(_.getClass.getSimpleName).getOrElse("None")
      s"EJEAPartialCompletionData(jobResult: ${jobResult.isDefined}, hashes: $hashesString)"
    }
}

// Can't do case-to-case inheritance so let's jump through these extractor-pattern hoops...
class EJEACompletionData(val jobResult: BackendJobExecutionResponse) extends EJEAData {
  override def toString = s"EJEACompletionData(${jobResult.getClass.getSimpleName})"
}
object EJEACompletionData {
  def apply(jobResult: BackendJobExecutionResponse) = new EJEACompletionData(jobResult)
  def unapply(completionData: EJEACompletionData) = Option(completionData.jobResult)
}

case class EJEASuccessfulCompletionDataWithHashes(override val jobResult: SucceededResponse, hashes: CallCacheHashes) extends EJEACompletionData(jobResult = jobResult) {
  override def toString = s"EJEACompletionDataWithHashes(${jobResult.getClass.getSimpleName}, ${hashes.hashes.size} hashes)"
}
