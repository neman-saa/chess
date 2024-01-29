package chess.http.routes

import java.util.UUID

import cats.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.effect.Concurrent
import cats.effect.Ref
import cats.implicits.*
import cats.syntax.all.*
import chess.core.Games
import chess.core.Lobby
import chess.core.Sessions
import chess.core.Users
import chess.domain.chessboard.Figure
import chess.domain.security.*
import chess.domain.socket.InputMessage
import chess.domain.socket.InputMessage.*
import chess.domain.user.User
import fs2.Pipe
import fs2.Stream
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.server.Router
import org.http4s.websocket.WebSocketFrame
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.Status
import org.typelevel.log4cats.Logger
import tsec.authentication.asAuthed
import tsec.authentication.SecuredRequestHandler
import tsec.authentication.TSecAuthService

class GamesRoute[F[_]: Concurrent](
    webSocketBuilder: WebSocketBuilder2[F],
    games: Games[F],
    lobby: Lobby[F],
    sessions: Sessions[F],
    users: Users[F],
    authenticator: Authenticator[F]
) extends Http4sDsl[F] {

  private val securedHandler: SecuredRequestHandler[F, String, User, JwtToken] = SecuredRequestHandler(authenticator)
  private object UserName extends QueryParamDecoderMatcher[String]("username")
  private object RoomId   extends QueryParamDecoderMatcher[String]("roomId")

  private val startLobby: AuthRoutes[F] = {
    case req @ GET -> Root / "startLobby" asAuthed user =>
      for {
        queue     <- Queue.unbounded[F, String]
        _         <- sessions.update(user.id, Some(queue))
        isCreated <- lobby.create(user.id)
        resp <- isCreated match {
          case Left(_) => Forbidden()
          case Right(id) =>
            val fromClient: Pipe[F, WebSocketFrame, Unit] = _ => fs2.Stream.empty[F]
            val toClient =
              (fs2.Stream.emit(s"Lobby created, waiting opponent, id: $id") ++
                fs2.Stream.fromQueueUnterminated(queue))
                .map(text => WebSocketFrame.Text(text))
            webSocketBuilder.build(toClient, fromClient)
        }
      } yield resp
  }

  private val connectToLobby: AuthRoutes[F] = {
    case req @ POST -> Root / "connectToLobby" :? RoomId(id) asAuthed user =>
      lobby.connectTo(UUID.fromString(id), user.id).flatMap {
        case Left(er) => BadRequest(er)
        case Right(player1) => Ok(s"Room created, connect to it, room id: $id")
      }
  }

  private val allLobbies: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / "allLobbies" =>
      for {
        lobbies <- lobby.allLobbies
        resp    <- Ok(lobbies)
      } yield resp
  }

  private val connectToGame: AuthRoutes[F] = {

    case req @ POST -> Root / "connectToGame" :? RoomId(id) asAuthed user =>
      games.connect(UUID.fromString(id), user.id).flatMap {
        case Left(e) => BadRequest(e)
        case Right(_) =>
          for {
            queue <- Queue.unbounded[F, String]
            _     <- sessions.update(user.id, Some(queue))
            resp <- {
              val fromClient: Pipe[F, WebSocketFrame, Unit] =
                (in: fs2.Stream[F, WebSocketFrame]) =>
                  in.collect {
                    case _: WebSocketFrame.Close     => CancelGame(UUID.fromString(id))
                    case WebSocketFrame.Text(str, _) => InputMessage.parse(user.id, str, UUID.fromString(id))
                  }.evalMap(games.processMessage)
              val toClient =
                Stream.emit("Connected to the game, waiting opponent.").map(WebSocketFrame.Text(_)) ++ fs2.Stream
                  .fromQueueUnterminated(queue)
                  .map(WebSocketFrame.Text(_))
              webSocketBuilder.build(toClient, fromClient)
            }
          } yield resp
      }
  }

  val unAuthedRoutes: HttpRoutes[F] = allLobbies
  val authedRoutes: HttpRoutes[F] = securedHandler.liftService(
    startLobby.restrictedTo(allRoles) |+|
    connectToLobby.restrictedTo(allRoles) |+|
    connectToGame.restrictedTo(allRoles)
  )

  val allRoutes: HttpRoutes[F] = Router(
    "/games" -> (unAuthedRoutes <+> authedRoutes)
  )
}

object GamesRoute {
  def apply[F[_]: Concurrent](
      webSocketBuilder: WebSocketBuilder2[F],
      games: Games[F],
      lobby: Lobby[F],
      sessions: Sessions[F],
      users: Users[F],
      authenticator: Authenticator[F]
  ): GamesRoute[F] = new GamesRoute[F](webSocketBuilder, games, lobby, sessions, users, authenticator)
}
