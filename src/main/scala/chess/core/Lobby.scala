package chess.core

import java.util.UUID

import cats.effect.std.Queue
import cats.effect.Concurrent
import cats.effect.Ref
import cats.syntax.all.*
import chess.core.Games
import org.typelevel.log4cats.Logger

trait Lobby[F[_]] {
  def create(player1: UUID): F[Either[String, UUID]]
  def connectTo(roomId: UUID, player2: UUID): F[Either[ConnectionError, UUID]]

  def exists(id: RoomId): F[Option[UUID]]
  def allLobbies: F[List[UUID]]
}

class LobbyLive[F[_]: Concurrent: Logger](games: Games[F], lobbies: Ref[F, Map[UUID, UUID]], sessions: Sessions[F])
    extends Lobby[F] {
  def create(player1: UUID): F[Either[String, UUID]] = for {
    lobbiess <- lobbies.get
    exists = lobbiess.values.exists(_ == player1)
    res <-
      if (exists) Logger[F].info("Creating existing lobby").map(_ => Left("You have already created a lobby"))
      else
        for {
          uuid <- UUID.randomUUID().pure[F]
          res  <- lobbies.updateAndGet(lobbies => lobbies + (uuid -> player1))
          _    <- Logger[F].info(s"created lobby $uuid, current lobbies are $res")
        } yield Right(uuid)
  } yield res

  def connectTo(roomId: UUID, player2: UUID): F[Either[ConnectionError, UUID]] = for {
    lobbiess <- lobbies.get
    message  <- lobbiess.get(roomId).pure[F]
    result <- message match {
      case None => Left(ConnectionError("No such room or invalid UUID")).pure[F]
      case Some(player1) =>
        sessions
          .offer(player1, s"go to localhost:8080/chess/games/connectToGame/$roomId/$player1 to start game")
          .flatMap(_ => lobbies.update(lobbies => lobbies - roomId))
          .flatMap(_ => games.startConnectingGame(roomId, player1, player2))
          .map(_ => Right(player1))
    }
  } yield result

  def exists(id: RoomId): F[Option[UUID]] = lobbies.get.map(map => Some(id).filter(map.contains))

  override def allLobbies: F[List[UUID]] = lobbies.get.map(_.keys.toList)
}

object LobbyLive {
  def apply[F[_]: Concurrent: Logger](games: Games[F], sessions: Sessions[F]): F[LobbyLive[F]] = for {
    ref <- Ref.of[F, Map[UUID, UUID]](Map.empty)
  } yield new LobbyLive(games, ref, sessions)
}

case class ConnectionError(message: String)
