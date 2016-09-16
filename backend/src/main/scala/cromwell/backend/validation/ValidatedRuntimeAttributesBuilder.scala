package cromwell.backend.validation

import cats.data.Validated.{Invalid, Valid}
import cromwell.backend.RuntimeAttributeDefinition
import cromwell.core._
import lenthall.exception.MessageAggregation
import org.slf4j.Logger
import wdl4s.values.WdlValue
import cats.implicits._

final case class ValidatedRuntimeAttributes(attributes: Map[String, Any])

/**
  * Uses a collection of `RuntimeAttributesValidation` to build a `ValidatedRuntimeAttributes`, or throw an exception
  * with all of the validation errors.
  */
trait ValidatedRuntimeAttributesBuilder {

  /**
    * Returns the validations that should be used and returned during evaluation.
    *
    * These validations will be checked on backend initialization against all the calls. If a validation value will not
    * be supported, but should be checked anyway, supply it in `valuesOnlyValidations`, not here.
    *
    * @return the validations that should be used and returned during evaluation.
    */
  def validations: Seq[RuntimeAttributesValidation[_]]

  /**
    * Returns a mapping of the validations: RuntimeAttributesValidation each converted to a RuntimeAttributeDefinition.
    */
  final lazy val definitions: Seq[RuntimeAttributeDefinition] = {
    validations map RuntimeAttributesValidation.toRuntimeAttributeDefinition
  }

  /**
    * Returns the additional validations that should be used during value parsing.
    *
    * For example, sometimes docker might not be supported, BUT we want to still validate the value if specified.
    *
    * In that case, return the validation here.
    *
    * @return the additional validations that should be used during value parsing.
    */
  protected def unsupportedExtraValidations: Seq[OptionalRuntimeAttributesValidation[_]] = Seq.empty

  def unsupportedKeys(keys: Seq[String]): Seq[String] = keys.diff(validationKeys)

  private lazy val validationKeys = validations.map(_.key)

  def build(attrs: Map[String, WdlValue], logger: Logger): ValidatedRuntimeAttributes = {
    RuntimeAttributesValidation.warnUnrecognized(attrs.keySet, validationKeys.toSet, logger)

    val runtimeAttributesErrorOr: ErrorOr[ValidatedRuntimeAttributes] = validate(attrs)
    runtimeAttributesErrorOr match {
      case Valid(runtimeAttributes) => runtimeAttributes
      case Invalid(nel) => throw new RuntimeException with MessageAggregation {
        override def exceptionContext: String = "Runtime attribute validation failed"

        override def errorMessages: Traversable[String] = nel.toList
      }
    }
  }

  private def validate(values: Map[String, WdlValue]): ErrorOr[ValidatedRuntimeAttributes] = {
    val validationsForValues: Seq[RuntimeAttributesValidation[_]] = validations ++ unsupportedExtraValidations
    val errorsOrValuesMap: Seq[(String, ErrorOr[Any])] =
      validationsForValues.map(validation => validation.key -> validation.validate(values))

    val emptyResult: ErrorOr[List[(String, Any)]] = List.empty[(String, Any)].validNel
    val validationResult = errorsOrValuesMap.foldLeft(emptyResult) { (agg, errorOrValue) =>
      agg |+| {
        errorOrValue match {
          case (key, Valid(value)) => List(key -> value).valid
          case (key, Invalid(nel)) => nel.invalid
        }
      }
    }

    validationResult.map(result => ValidatedRuntimeAttributes(result.toMap))
  }
}
