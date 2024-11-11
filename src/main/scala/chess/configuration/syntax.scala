package chess.configuration

import scala.reflect.ClassTag

import cats.implicits.*
import cats.MonadError
import cats.MonadThrow
import pureconfig.error.ConfigReaderException
import pureconfig.ConfigReader
import pureconfig.ConfigSource

object syntax {
  extension (source: ConfigSource) {
    def loadF[F[_], A](using reader: ConfigReader[A], F: MonadThrow[F], tag: ClassTag[A]): F[A] =
      F.pure(source.load[A]).flatMap {
        case Left(errors) => F.raiseError[A](ConfigReaderException(errors))

        case Right(value) => F.pure(value)
      }
  }
}
