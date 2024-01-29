package chess.validation

import cats.data.Validated.{Valid, Invalid}
import cats.data.ValidatedNel
import cats.effect.Concurrent
import chess.validation.validators.{ValidationFailure, Validator}
import org.typelevel.log4cats.Logger
import cats.syntax.all.*
import org.http4s.{EntityDecoder, Request, Response}
import org.http4s.dsl.Http4sDsl
import chess.logging.syntax.*
object syntax {

  private def validateEntity[A](entity: A)(using validator: Validator[A]): ValidatedNel[ValidationFailure, A] = validator.validate(entity)

  trait HttpValidationDsl[F[_]: Concurrent: Logger] extends Http4sDsl[F] {

    extension (req: Request[F])
      def validate[A: Validator](
          serverLogicIfValid: A => F[Response[F]])(using EntityDecoder[F, A]): F[Response[F]] =
        req
          .as[A]
          .logError(e => s"Parsing payload failed: $e")
          .map(validateEntity)
          .flatMap {
            case Valid(entity) => serverLogicIfValid(entity)
            case Invalid(errors) => BadRequest(errors.toList.mkString("{", ", ", "}"))
          }
  }
}
