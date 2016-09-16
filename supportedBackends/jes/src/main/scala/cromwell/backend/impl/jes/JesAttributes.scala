package cromwell.backend.impl.jes

import java.net.URL

import com.typesafe.config.Config
import cromwell.backend.impl.jes.JesImplicits.GoogleAuthWorkflowOptions
import cromwell.core.{ErrorOr, WorkflowOptions}
import cromwell.filesystems.gcs.{GoogleAuthMode, GoogleConfiguration}
import lenthall.config.ScalaConfig._
import lenthall.config.ValidatedConfig._
import wdl4s.ExceptionWithErrors

import scala.language.postfixOps
import cats.data._
import cats.data.Validated._
import cats.implicits._
import cats.syntax.AllSyntax
import cats.instances.AllInstances

case class JesAttributes(project: String,
                         genomicsAuth: GoogleAuthMode,
                         gcsFilesystemAuth: GoogleAuthMode,
                         executionBucket: String,
                         endpointUrl: URL,
                         maxPollingInterval: Int) {

  def assertWorkflowOptions(options: WorkflowOptions): Unit = {
    // These methods throw on bad options
    genomicsAuth.assertWorkflowOptions(options.toGoogleAuthOptions)
    gcsFilesystemAuth.assertWorkflowOptions(options.toGoogleAuthOptions)
  }
}

object JesAttributes {

  private val jesKeys = Set(
    "project",
    "root",
    "maximum-polling-interval",
    "dockerhub",
    "genomics",
    "filesystems",
    "genomics.auth",
    "genomics.endpoint-url",
    "filesystems.gcs.auth"
  )

  private val context = "Jes"

  def apply(googleConfig: GoogleConfiguration, backendConfig: Config): JesAttributes = {
    backendConfig.warnNotRecognized(jesKeys, context)

    val project: ValidatedNel[String, String] = backendConfig.validateString("project")
    val executionBucket: ValidatedNel[String, String] = backendConfig.validateString("root")
    val endpointUrl: ErrorOr[URL] = backendConfig.validateURL("genomics.endpoint-url")
    val maxPollingInterval: Int = backendConfig.getIntOption("maximum-polling-interval").getOrElse(600)
    val genomicsAuthName: ErrorOr[String] = backendConfig.validateString("genomics.auth")
    val gcsFilesystemAuthName: ErrorOr[String] = backendConfig.validateString("filesystems.gcs.auth")

    (project |@| executionBucket |@| endpointUrl |@| genomicsAuthName |@| gcsFilesystemAuthName) map {
      (_, _, _, _, _)
    } andThen { case (p, b, u, genomicsName, gcsName) =>
      (googleConfig.auth(genomicsName) |@| googleConfig.auth(gcsName)) map { case (genomicsAuth, gcsAuth) =>
        JesAttributes(p, genomicsAuth, gcsAuth, b, u, maxPollingInterval)
      }
    } match {
      case Valid(r) => r
      case Invalid(f) =>
        throw new IllegalArgumentException with ExceptionWithErrors {
          override val message = "Jes Configuration is not valid: Errors"
          override val errors = f
        }
    }
  }
}
