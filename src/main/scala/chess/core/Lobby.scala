package chess.core

import cats.effect.{Concurrent, Ref}
import chess.core.Games
import java.util.UUID
import cats.effect.std.Queue
import cats.syntax.all.*
trait Lobby[F[_]]{
  def create(player1: UUID): F[Either[String,UUID]]
  def connectTo(roomId: UUID, player2: UUID): F[Either[ConnectionError, UUID]]

  def exists(id: RoomId): F[Option[UUID]]
  def allLobbies: F[List[UUID]]
}

class LobbyLive[F[_]: Concurrent](
  games: Games[F],
  lobbies: Ref[F, Map[UUID, UUID]],
  link: String,
  sessions: Sessions[F]) extends Lobby[F] {
  def create(player1: UUID): F[Either[String, UUID]] = for {
    lobbiess <- lobbies.get
    exists = lobbiess.values.exists(_ == player1)
    res <-
      if(exists) Left("You have already created a lobby").pure[F]
      else for {
        uuid <- UUID.randomUUID().pure[F]
        _ <- lobbies.update(lobbies => lobbies + (uuid -> player1))
      } yield Right(uuid)
  } yield res

  def connectTo(roomId: UUID, player2: UUID): F[Either[ConnectionError, UUID]] = for {
    lobbiess <- lobbies.get
    message <- lobbiess.get(roomId).pure[F]
    result <- message match {
      case None => Left(ConnectionError("No such room or invalid UUID")).pure[F]
      case Some(player1) =>
        sessions.offer(player1, s"go to $link/$roomId/$player1 to start game")
          .flatMap(_ => lobbies.update(lobbies => lobbies - roomId))
          .flatMap(_ => games.startConnectingGame(roomId, player1, player2))
          .map(_ => Right(player1))
    }
  } yield result

  def exists(id: RoomId): F[Option[UUID]] = lobbies.get.map(map => Some(id).filter(map.contains))

  override def allLobbies: F[List[UUID]] = lobbies.get.map(_.keys.toList)
}

case class ConnectionError(message: String)