package cromwell.engine.workflow.workflowstore

import java.time.OffsetDateTime

import akka.actor.{ActorLogging, ActorRef, LoggingFSM, Props}
import cromwell.core.{WorkflowId, WorkflowMetadataKeys, WorkflowSourceFiles}
import cromwell.engine.workflow.WorkflowManagerActor
import cromwell.engine.workflow.WorkflowManagerActor.WorkflowNotFoundException
import cromwell.engine.workflow.workflowstore.WorkflowStoreActor._
import cromwell.engine.workflow.workflowstore.WorkflowStoreState.StartableState
import cromwell.services.metadata.{MetadataEvent, MetadataKey, MetadataValue}
import cromwell.services.metadata.MetadataService.{MetadataPutAcknowledgement, PutMetadataAction}
import org.apache.commons.lang3.exception.ExceptionUtils

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}
import scalaz.NonEmptyList

case class WorkflowStoreActor(store: WorkflowStore, serviceRegistryActor: ActorRef)
  extends LoggingFSM[WorkflowStoreActorState, WorkflowStoreActorData] with ActorLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  startWith(Unstarted, WorkflowStoreActorData(None, List.empty))
  self ! InitializerCommand

  when(Unstarted) {
    case Event(InitializerCommand, _) =>
      val work = store.initialize map { _ =>
        log.debug("Workflow store initialization successful")
      }
      addWorkCompletionHooks(InitializerCommand, work)
      goto(Working) using stateData.withCurrentCommand(InitializerCommand, sender)
    case Event(x: WorkflowStoreActorCommand, _) =>
      stay using stateData.withPendingCommand(x, sender)
  }

  when(Idle) {
    case Event(cmd: WorkflowStoreActorCommand, _) =>
      if (stateData.currentOperation.nonEmpty || stateData.pendingOperations.nonEmpty) {
        log.error("Non-empty WorkflowStoreActorData when in Idle state: {}", stateData)
      }
      startNewWork(cmd, sender, stateData.withCurrentCommand(cmd, sender))
  }

  when(Working) {
    case Event(WorkDone, data) =>
      val newData = data.pop
      newData.currentOperation match {
        case None => goto(Idle) using newData
        case Some(WorkflowStoreActorCommandWithSender(cmd, sndr)) => startNewWork(cmd, sndr, newData)
      }
    case Event(cmd: WorkflowStoreActorCommand, data) => stay using data.withPendingCommand(cmd, sender)
  }

  whenUnhandled {
    case Event(MetadataPutAcknowledgement(_), _) =>
      stay // Ignored
    case Event(msg, _) =>
      log.warning("Unexpected message to WorkflowStoreActor in state {} with data {}: {}", stateName, stateData, msg)
      stay
  }

  onTransition {
    case fromState -> toState =>
      log.debug("WorkflowStore moving from {} (using {}) to {} (using {})", fromState, stateData, toState, nextStateData)
  }

  private def startNewWork(command: WorkflowStoreActorCommand, sndr: ActorRef, nextData: WorkflowStoreActorData) = {
    val work: Future[Any] = command match {
      case cmd @ SubmitWorkflow(sourceFiles) =>
        store.add(NonEmptyList(sourceFiles)) map { ids =>
          val id = ids.head
          registerSubmissionWithMetadataService(id, sourceFiles)
          sndr ! WorkflowSubmittedToStore(id)
          log.info("Workflow {} submitted.", id)
        }
      case cmd @ BatchSubmitWorkflows(sources) =>
        store.add(sources) map { ids =>
          val assignedSources = ids.zip(sources)
          assignedSources foreach { case (id, sourceFiles) => registerSubmissionWithMetadataService(id, sourceFiles) }
          sndr ! WorkflowsBatchSubmittedToStore(ids)
          log.info("Workflows {} submitted.", ids.list.toList.mkString(", "))
        }
      case cmd @ FetchRunnableWorkflows(n) =>
        newWorkflowMessage(n) map { nwm =>
          nwm match {
            case NewWorkflowsToStart(workflows) => log.info("{} new workflows fetched", workflows.size)
            case NoNewWorkflowsToStart => log.debug("No workflows fetched")
            case _ => log.error("Unexpected response from newWorkflowMessage({}): {}", n, nwm)
          }
          sndr ! nwm
        }
      case cmd @ AbortWorkflow(id, manager) =>
        store.remove(id) map { removed =>
          if (removed) {
            log.debug(s"Workflow $id aborted and removed from the workflow store.")
            manager ! WorkflowManagerActor.AbortWorkflowCommand(id, sndr)
          } else {
            sndr ! WorkflowAbortFailed(id, new WorkflowNotFoundException(s"Couldn't abort $id because no workflow with that ID is in progress"))
          }
        }
      case cmd @ RemoveWorkflow(id) =>
        store.remove(id) map { removed =>
          if (removed) {
            log.debug("Workflow {} removed from store successfully.", id)
          } else {
            log.warning(s"Attempted to remove ID {} from the WorkflowStore but it didn't exist", id)
          }
        }
      case oops =>
        log.error("Unexpected type of start work command: {}", oops.getClass.getSimpleName)
        Future.successful(self ! WorkDone)
    }
    addWorkCompletionHooks(command, work)
    goto(Working) using nextData
  }

  private def addWorkCompletionHooks[A](command: WorkflowStoreActorCommand, work: Future[A]) = {
    work.onComplete {
      case Success(_) =>
        self ! WorkDone
      case Failure(t) =>
        log.error("Error occurred during {}: {} because {}", command.getClass.getSimpleName, t.toString, ExceptionUtils.getStackTrace(t))
        self ! WorkDone
    }
  }

  /**
    * Fetches at most n workflows, and builds the correct response message based on if there were any workflows or not
    */
  private def newWorkflowMessage(maxWorkflows: Int): Future[WorkflowStoreActorResponse] = {
    def fetchRunnableWorkflowsIfNeeded(maxWorkflowsInner: Int, state: StartableState) = {
      if (maxWorkflows > 0) {
        store.fetchRunnableWorkflows(maxWorkflowsInner, state)
      } else {
        Future.successful(List.empty[WorkflowToStart])
      }
    }

    val runnableWorkflows = for {
      restartableWorkflows <- fetchRunnableWorkflowsIfNeeded(maxWorkflows, WorkflowStoreState.Restartable)
      submittedWorkflows <- fetchRunnableWorkflowsIfNeeded(maxWorkflows - restartableWorkflows.size, WorkflowStoreState.Submitted)
    } yield restartableWorkflows ++ submittedWorkflows

    runnableWorkflows map {
      case x :: xs => NewWorkflowsToStart(NonEmptyList.nels(x, xs: _*))
      case _ => NoNewWorkflowsToStart
    }
  }

  /**
    * Takes the workflow id and sends it over to the metadata service w/ default empty values for inputs/outputs
    */
  private def registerSubmissionWithMetadataService(id: WorkflowId, sourceFiles: WorkflowSourceFiles): Unit = {
    val submissionEvents = List(
      MetadataEvent(MetadataKey(id, None, WorkflowMetadataKeys.SubmissionTime), MetadataValue(OffsetDateTime.now.toString)),
      MetadataEvent.empty(MetadataKey(id, None, WorkflowMetadataKeys.Inputs)),
      MetadataEvent.empty(MetadataKey(id, None, WorkflowMetadataKeys.Outputs)),

      MetadataEvent(MetadataKey(id, None, WorkflowMetadataKeys.SubmissionSection, WorkflowMetadataKeys.SubmissionSection_Workflow), MetadataValue(sourceFiles.wdlSource)),
      MetadataEvent(MetadataKey(id, None, WorkflowMetadataKeys.SubmissionSection, WorkflowMetadataKeys.SubmissionSection_Inputs), MetadataValue(sourceFiles.inputsJson)),
      MetadataEvent(MetadataKey(id, None, WorkflowMetadataKeys.SubmissionSection, WorkflowMetadataKeys.SubmissionSection_Options), MetadataValue(sourceFiles.workflowOptionsJson))
    )

    serviceRegistryActor ! PutMetadataAction(submissionEvents)
  }
}

object WorkflowStoreActor {

  private[workflowstore] case class WorkflowStoreActorCommandWithSender(command: WorkflowStoreActorCommand, sender: ActorRef)

  private[workflowstore] case class WorkflowStoreActorData(currentOperation: Option[WorkflowStoreActorCommandWithSender], pendingOperations: List[WorkflowStoreActorCommandWithSender]) {
    def withCurrentCommand(command: WorkflowStoreActorCommand, sender: ActorRef) = this.copy(currentOperation = Option(WorkflowStoreActorCommandWithSender(command, sender)))
    def withPendingCommand(newCommand: WorkflowStoreActorCommand, sender: ActorRef) = this.copy(pendingOperations = this.pendingOperations :+ WorkflowStoreActorCommandWithSender(newCommand, sender))
    def pop = {
      if (pendingOperations.isEmpty) { WorkflowStoreActorData(None, List.empty) }
      else { WorkflowStoreActorData(Option(pendingOperations.head), pendingOperations.tail) }
    }
  }

  private[workflowstore] sealed trait WorkflowStoreActorState
  private[workflowstore] case object Unstarted extends WorkflowStoreActorState
  private[workflowstore] case object Working extends WorkflowStoreActorState
  private[workflowstore] case object Idle extends WorkflowStoreActorState

  sealed trait WorkflowStoreActorCommand
  final case class SubmitWorkflow(source: WorkflowSourceFiles) extends WorkflowStoreActorCommand
  final case class BatchSubmitWorkflows(sources: NonEmptyList[WorkflowSourceFiles]) extends WorkflowStoreActorCommand
  final case class FetchRunnableWorkflows(n: Int) extends WorkflowStoreActorCommand
  final case class RemoveWorkflow(id: WorkflowId) extends WorkflowStoreActorCommand
  final case class AbortWorkflow(id: WorkflowId, manager: ActorRef) extends WorkflowStoreActorCommand

  private case object InitializerCommand extends WorkflowStoreActorCommand
  private case object WorkDone

  sealed trait WorkflowStoreActorResponse
  final case class WorkflowSubmittedToStore(workflowId: WorkflowId) extends WorkflowStoreActorResponse
  final case class WorkflowsBatchSubmittedToStore(workflowIds: NonEmptyList[WorkflowId]) extends WorkflowStoreActorResponse
  case object NoNewWorkflowsToStart extends WorkflowStoreActorResponse
  final case class NewWorkflowsToStart(workflows: NonEmptyList[WorkflowToStart]) extends WorkflowStoreActorResponse
  final case class WorkflowAborted(workflowId: WorkflowId) extends WorkflowStoreActorResponse
  final case class WorkflowAbortFailed(workflowId: WorkflowId, reason: Throwable) extends WorkflowStoreActorResponse

  def props(workflowStoreDatabase: WorkflowStore, serviceRegistryActor: ActorRef) = {
    Props(WorkflowStoreActor(workflowStoreDatabase, serviceRegistryActor))
  }
}
