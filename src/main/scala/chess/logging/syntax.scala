package chess.logging

import cats.MonadError
import org.typelevel.log4cats.Logger

object syntax {
  extension [F[_], E, A](fa: F[A])(using me: MonadError[F, E], logger: Logger[F]) {
    def log(success: A => String, error: E => String): F[A] = me.attemptTap(fa) {
      case Left(e)      => logger.error(error(e))
      case Right(value) => logger.info(success(value))
    }

    def logError(error: E => String): F[A] = me.attemptTap(fa) {
      case Left(e)  => logger.error(error(e))
      case Right(_) => me.pure(())
    }
  }
}
