package chess.playground

import cats.Traverse
import cats.effect.std.Queue
import cats.effect.{Concurrent, Deferred, ExitCode, IO, IOApp, Resource}
import fs2.{Pipe, Stream}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.syntax.traverse.*
import cats.instances.all.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.server.Router
import org.http4s.websocket.WebSocketFrame
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.Status
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port

import scala.concurrent.duration.*

object Playground extends IOApp.Simple {
  override def run: IO[Unit] = {

    class SimpleRoutes(webSocketBuilder: WebSocketBuilder2[IO], queue: Queue[IO, String]) extends Http4sDsl[IO] {

      val dsl = Http4sDsl[IO]

      import dsl.*
      val simpleWsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
        case GET -> Root / "wstest" =>
          val fromClient: Pipe[IO, WebSocketFrame, Unit] = (in: fs2.Stream[IO, WebSocketFrame]) =>
            in.collect {
              case WebSocketFrame.Text("ping", _) => {println("received"); "ping"}
            }.map(queue.offer)

          val toClient: Stream[IO, WebSocketFrame] = (fs2.Stream.emit("Started") ++ fs2.Stream.fromQueueUnterminated(queue).map(_ => "pong")).map(WebSocketFrame.Text(_))
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

    serverResource.use(_ => IO.println("server started") *> IO.never)
  }
}
