package chess.playground

import scala.concurrent.duration._

import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import fs2.Pipe
import fs2.Stream
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

object Playground extends IOApp.Simple {

  class SimpleRoutes(webSocketBuilder: WebSocketBuilder2[IO], queue: Queue[IO, String]) extends Http4sDsl[IO] {
    val dsl = Http4sDsl[IO]

    import dsl._

    val simpleWsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case GET -> Root / "wstest" =>
        val fromClient: Pipe[IO, WebSocketFrame, Unit] = (in: Stream[IO, WebSocketFrame]) =>
          in.collect {
            case WebSocketFrame.Text("ping", _) =>
              println("received ping")
              "pong"
          }.evalMap(queue.offer)

        val toClient: Stream[IO, WebSocketFrame] =
          Stream(
            Stream.emit("Started"),
            Stream.fromQueueUnterminated(queue),
            Stream.awakeEvery[IO](30.seconds).map(_ => "keep alive")
          ).parJoinUnbounded.map(WebSocketFrame.Text(_))

        webSocketBuilder.build(toClient, fromClient)
    }
  }

  val serverResource = for {
    queue <- Resource.eval(Queue.unbounded[IO, String])
    server <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("0.0.0.0").get)
      .withHttpWebSocketApp(wsb => new SimpleRoutes(wsb, queue).simpleWsRoute.orNotFound)
      .build
  } yield server

  override def run: IO[Unit] = serverResource.useForever
}