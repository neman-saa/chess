package chess.playground

import scala.concurrent.duration.*

import cats.effect._
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.effect.Concurrent
import cats.instances.all.*
import cats.syntax.all.*
import cats.Traverse
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import fs2.Pipe
import fs2.Stream
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.server.Router
import org.http4s.websocket.WebSocketFrame
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.Status

object Playground extends IOApp.Simple {
  override def run: IO[Unit] = {

    class SimpleRoutes(webSocketBuilder: WebSocketBuilder2[IO], queue: Queue[IO, String]) extends Http4sDsl[IO] {

      val dsl = Http4sDsl[IO]

      import dsl.*
      val simpleWsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
        case GET -> Root / "wstest" =>
          val fromClient: Pipe[IO, WebSocketFrame, Unit] = (in: fs2.Stream[IO, WebSocketFrame]) =>
            in.collect {
              case WebSocketFrame.Text("ping", _) => { println("received"); "ping" }
            }.evalMap(queue.offer)

          val toClient: Stream[IO, WebSocketFrame] =
            (fs2.Stream.emit("Started") ++ fs2.Stream.fromQueueUnterminated(queue).map(_ => "pong"))
              .map(WebSocketFrame.Text(_))
          webSocketBuilder.build(toClient, fromClient)
      }
    }

    val serverResource = for {
      queue <- Resource.eval(Queue.unbounded[IO, String])
      server <- EmberServerBuilder
        .default[IO]
        .withPort(Port.fromInt(8080).get)
        .withHost(Host.fromString("0.0.0.0").get)
        .withHttpWebSocketApp(wsbuilder => (new SimpleRoutes(wsbuilder, queue)).simpleWsRoute.orNotFound)
        .build
    } yield server

    // serverResource.use(_ => IO.println("server started") *> IO.never)

    var current = 1000000000
    def oneToBillion: Unit =
      if (current == 0) ()
      else { current = current - 1; oneToBillion }

    def withRef1(ref: Ref[IO, Int]): IO[Unit] = for {
      n <- ref.updateAndGet(_ - 1)
      res <- n match {
        case 0 => IO.unit
        case _ => withRef1(ref)
      }
    } yield res

    def withRef2(ref: Ref[IO, Int]): IO[Unit] = ref.updateAndGet(_ - 1).flatMap {
      case 0 => IO.unit
      case _ => withRef2(ref)
    }

    for {
      ref        <- Ref.of[IO, Int](1000000000)
      timeFirst  <- IO(System.currentTimeMillis())
      _          <- withRef1(ref)
      timeSecond <- IO(System.currentTimeMillis())
    } yield println(timeSecond - timeFirst)

    List(1, 2, 3).flatTraverse(x => IO(if (x % 2 == 0) List(x * 2) else Nil)).map(println)

        for {
          _ <- Stream.awakeEvery[IO](1.second).compile.drain.start
          _ <- IO.println(5)
        } yield ()
  }

}