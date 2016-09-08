package cromwell.filesystems

import cats.data._

package object gcs {
  type ErrorOr[+A] = Validated[NonEmptyList[String], A]
  type RefreshToken = String
}
