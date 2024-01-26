package chess.http.routes

import io.circe.generic.auto.*
import io.circe.syntax.*
import cats.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.effect.{Concurrent, Ref}
import cats.implicits.*
import cats.syntax.all.*
import cats.effect.syntax.all.*
import chess.core.{Games, Lobby, Sessions, Users}
import chess.domain.chessboard.Figure
import chess.domain.user.User
import org.http4s.{HttpRoutes, Response, Status}
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.server.Router
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.typelevel.log4cats.Logger
import fs2.{Pipe, Stream}
import tsec.authentication.{SecuredRequestHandler, asAuthed}
import chess.domain.socket.InputMessage
import chess.domain.socket.InputMessage.*

import java.util.UUID

class GamesRoute[F[_]: Concurrent](webSocketBuilder: WebSocketBuilder2[F], games: Games[F], lobby: Lobby[F], sessions: Sessions[F], users: Users[F])
  extends Http4sDsl[F] {

  private object UserName extends QueryParamDecoderMatcher[String]("username")
  private object RoomId extends QueryParamDecoderMatcher[String]("roomId")

  private val startLobby: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / "startLobby" :? UserName(name) =>
      users.findByNick(name).flatMap {
        case None => Response(Status.Unauthorized).pure[F]
        case Some(user) => for {
          queue <- Queue.unbounded[F, String]
          _ <- sessions.update(user.id, Some(queue))
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
  }

  private val connectToLobby: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "connectToLobby" :? RoomId(id) +& UserName(player2) =>
      users.findByNick(player2).flatMap {
        case None => Response(Status.Unauthorized).pure[F]
        case Some(user) =>
          lobby.connectTo(UUID.fromString(id), user.id).flatMap{
            case Left(er) => BadRequest(er)
            case Right(player1) => for {
              _ <- sessions.offer(player1, "Room created,  connect to it")
              resp <- Ok("Room created, connect to it")
            } yield resp
          }
      }
  }

  private val allLobbies: HttpRoutes[F] = HttpRoutes.of[F]{
    case req @ GET -> Root / "allLobbies" => for {
      lobbies <- lobby.allLobbies
      resp <- Ok(lobbies)
    } yield resp
  }

  private val connectToGame: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case req @ POST -> Root / "connectToGame" :? RoomId(id) +& UserName(name) =>
        users.findByNick(name).flatMap {
          case None => Response(Status.Unauthorized).pure[F]
          case Some(user) =>
            games.connect(UUID.fromString(id), user.id).flatMap {
              case Left(e) => BadRequest(e)
              case Right(_) => for {
                queue <- Queue.unbounded[F, String]
                _ <- sessions.update(user.id, Some(queue))
                resp <- {
                  val fromClient: Pipe[F, WebSocketFrame, Unit] =
                    (in: fs2.Stream[F, WebSocketFrame]) =>
                    in.collect{
                      case _: WebSocketFrame.Close => CancelGame(UUID.fromString(id))
                      case WebSocketFrame.Text(str, _) => InputMessage.parse(user.id, str, UUID.fromString(id))
                    }.evalMap(games.processMessage)
                  val toClient = Stream.emit("Connected to the game, waiting opponent.").map(WebSocketFrame.Text(_)) ++ fs2.Stream.fromQueueUnterminated(queue).map(WebSocketFrame.Text(_))
                  webSocketBuilder.build(toClient, fromClient)
                }
              } yield resp
            }
        }
    }
  }
}

object GamesRoute {
  def apply[F[_]: Concurrent](
      webSocketBuilder: WebSocketBuilder2[F],
      games: Games[F],
      lobby: Lobby[F],
      sessions: Sessions[F],
      users: Users[F]
  ): F[GamesRoute[F]] = new GamesRoute[F](webSocketBuilder, games, lobby, sessions, users).pure[F]
}
