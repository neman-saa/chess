package chess.validation

import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.effect.Concurrent
import cats.syntax.all.*
import chess.logging.syntax.*
import chess.validation.validators.ValidationFailure
import chess.validation.validators.Validator
import org.http4s.dsl.Http4sDsl
import org.http4s.EntityDecoder
import org.http4s.Request
import org.http4s.Response
import org.typelevel.log4cats.Logger
object syntax {

  private def validateEntity[A](entity: A)(using validator: Validator[A]): ValidatedNel[ValidationFailure, A] =
    validator.validate(entity)

  trait HttpValidationDsl[F[_]: Concurrent: Logger] extends Http4sDsl[F] {

    extension (req: Request[F])
      def validate[A: Validator](serverLogicIfValid: A => F[Response[F]])(using EntityDecoder[F, A]): F[Response[F]] =
        req
          .as[A]
          .logError(e => s"Parsing payload failed: $e")
          .map(validateEntity)
          .flatMap {
            case Valid(entity)   => serverLogicIfValid(entity)
            case Invalid(errors) => BadRequest(errors.toList.mkString("{", ", ", "}"))
          }
  }
}
