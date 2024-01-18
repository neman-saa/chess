package chess.core

import cats.Traverse
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import chess.domain.chessboard.defaultBoard
import chess.domain.game.*
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.instances.list.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*
import chess.core.GameWorker.*

import java.util.UUID

type Coordinate = (Int, Int)
type GameStatus = String

trait Games[F[_]] {

  def allGames: F[List[UUID]]
  def persistGameAndDeleteFromList(id: UUID, winner: Option[String]): F[Unit]
  def cancelGame(id: UUID): F[Unit]
  def moveFor(id: UUID, move: Move): F[GameStatus]
  def startGame(players: (UUID, UUID)): F[UUID]
  def getGameById(id: UUID): F[Option[GameForPersistence]]
}

class LiveGames[F[_]: Concurrent: Logger](xa: Transactor[F])(games: Ref[F, Map[UUID, ((UUID, UUID), GameWorker[F])]]) extends Games[F] {

  def allGames: F[List[UUID]] = games.get.map(map => map.keys.toList)

  def persistGameAndDeleteFromList(id: UUID, winner: Option[String]): F[Unit] = for {
    workers <- games.get
    worker <- workers(id).pure[F]
    _ <-
      sql"""
        INSERT INTO games (
        id,
        winner,
        player1,
        player2
        ) VALUES (
        $id,
        $winner,
        ${worker._1._1},
        ${worker._1._2}
        )
        """.update.run.transact(xa)
    _ <- games.update(map => map.removed(id))
  } yield ()


  def cancelGame(id: UUID): F[Unit] = games.update(map => map.removed(id))

  def moveFor(id: UUID, move: Move): F[GameStatus] = for {
    workers <- games.get
    worker <- workers(id)._2.pure[F]
    status <- worker.makeMove(move)
    isEnded <- status match {
      case "gameContinue" => false.pure[F]
      case status => persistGameAndDeleteFromList(id, Some(status)).map(_ => true)
    }
  } yield status

  def startGame(players: (UUID, UUID)): F[UUID] = {
    val id = UUID.randomUUID()

    for {
      worker <- GameWorker(players, id)
      _ <- games.update(map => map + (id -> (players, worker)))
    } yield id
  }

  override def getGameById(id: UUID): F[Option[GameForPersistence]] =
    sql"""
         SELECT * FROM games WHERE games.id = $id
         """
      .query[GameForPersistence]
      .option
      .transact(xa)
}

object LiveGames {

  given gamesRead: Read[GameForPersistence] = Read[(UUID, Option[String], UUID, UUID)].map {
    case (id: UUID, winner: Option[String], player1: UUID, player2: UUID) => GameForPersistence(id, winner, player1, player2)
  }

  def apply[F[_]: Concurrent: Logger](xa: Transactor[F]): F[LiveGames[F]] = for {
    workers <- Ref.of[F, Map[UUID, ((UUID, UUID), GameWorker[F])]](Map())
  } yield new LiveGames[F](xa)(workers)
}

