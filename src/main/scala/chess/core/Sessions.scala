package chess.core

import java.util.UUID

import cats.effect.kernel.Concurrent
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.instances.list.*
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

trait Sessions[F[_]] {
  def add(id: UUID): F[Unit]
  def delete(id: UUID): F[Unit]
  def update(id: UUID, sock: Option[Queue[F, String]]): F[Unit]
  def getSock(id: UUID): F[Option[PlayerSession[F]]]
  def offer(id: UUID, message: String): F[Unit]
}

class SessionsLive[F[_]: Concurrent: Logger](sessions: Ref[F, Map[UUID, PlayerSession[F]]]) extends Sessions[F] {

  def add(playerId: UUID): F[Unit] = for {
    res <- sessions.updateAndGet(_ + (playerId -> PlayerSession(playerId, None)))
    _   <- Logger[F].info(s"added $playerId session, current sessions are: $res")
  } yield ()

  def update(id: UUID, sock: Option[Queue[F, String]]): F[Unit] = for {
    res <- sessions.updateAndGet(_ + (id -> PlayerSession(id, sock)))
    _   <- Logger[F].info(s"updated user $id session, current sessions are: $res")
  } yield ()

  def delete(id: UUID): F[Unit] = for {
    res <- sessions.updateAndGet(_ - id)
    _   <- Logger[F].info(s"deleted user $id session, current sessions are: $res")
  } yield ()

  def getSock(id: UUID): F[Option[PlayerSession[F]]] = sessions.get.map(_.get(id))

  def offer(id: UUID, message: String): F[Unit] = for {
    sessionss <- sessions.get
    mbSock    <- sessionss.get(id).pure[F]
    _ <- mbSock match {
      case Some(session) => session.offer(message)
      case None          => ().pure[F]
    }
  } yield ()

  def offerForAll(message: String): F[Unit] = for {
    sessionss <- sessions.get
    _         <- sessionss.values.toList.traverse(_.offer(message))
  } yield ()
}

object SessionsLive {
  def apply[F[_]: Concurrent: Logger]: F[SessionsLive[F]] = for {
    sessions <- Ref.of[F, Map[UUID, PlayerSession[F]]](Map.empty)
  } yield new SessionsLive(sessions)
}

case class PlayerSession[F[_]: Concurrent](playerId: UUID, sock: Option[Queue[F, String]]) {
  def offer(message: String): F[Unit] =
    sock match
      case Some(sock) => sock.offer(message)
      case _          => ().pure[F]

}
