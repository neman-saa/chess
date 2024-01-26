package chess.playground

import cats.effect.{Deferred, ExitCode, IO, IOApp}
import fs2.{Pipe, Stream}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import scala.concurrent.duration.*

object Playground extends IOApp.Simple {

  override def run: IO[Unit] = {
    for {
      deferred <- Deferred[IO, Either[Throwable, Unit]]
      interrupter <- IO.sleep(2.second) >> deferred.complete(Left(new Exception()))
      stream <- Stream.eval(IO.sleep(3.second) >> IO.println(5)).interruptWhen(deferred).compile.drain
    } yield ()
  }

}
