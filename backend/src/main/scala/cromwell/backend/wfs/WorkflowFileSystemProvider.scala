package cromwell.backend.wfs

import java.nio.file.FileSystem

import com.typesafe.config.Config
import cromwell.backend.io.WorkflowPaths
import cromwell.backend.{BackendConfigurationDescriptor, BackendWorkflowDescriptor}
import cromwell.core.WorkflowOptions
import lenthall.config.ScalaConfig._

import scala.concurrent.ExecutionContext

object WorkflowFileSystemProvider {
  def workflowPaths(configurationDescriptor: BackendConfigurationDescriptor,
                    workflowDescriptor: BackendWorkflowDescriptor,
                    providers: Traversable[WorkflowFileSystemProvider],
                    fileSystemExecutionContext: ExecutionContext): WorkflowPaths = {
    val backendConfig = configurationDescriptor.backendConfig
    val fileSystemConfig = backendConfig.getConfigOr("filesystems")
    val globalConfig = configurationDescriptor.globalConfig
    val params = WorkflowFileSystemProviderParams(fileSystemConfig, globalConfig, workflowDescriptor.workflowOptions,
      fileSystemExecutionContext)
    val fileSystems = providers.flatMap(_.fileSystemOption(params)).toList
    new WorkflowPaths(workflowDescriptor, configurationDescriptor.backendConfig, fileSystems)
  }
}

final case class WorkflowFileSystemProviderParams(fileSystemConfig: Config, globalConfig: Config,
                                                  workflowOptions: WorkflowOptions,
                                                  fileSystemExecutionContext: ExecutionContext)

trait WorkflowFileSystemProvider {
  def fileSystemOption(params: WorkflowFileSystemProviderParams): Option[FileSystem]
}
