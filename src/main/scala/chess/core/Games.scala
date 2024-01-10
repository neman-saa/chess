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

  def allGames: F[List[Game]]
  def persistGameAndDeleteFromList(id: UUID, winner: Option[String]): F[Unit]
  def cancelGame(id: UUID): F[Unit]
  def moveFor(id: UUID, fromTo: (Coordinate, Coordinate)): F[Boolean]
  def startGame(players: (UUID, UUID)): F[UUID]
  def getGameById(id: UUID): F[Option[GameForPersistence]]
}

class LiveGames[F[_]: Concurrent: Logger](xa: Transactor[F])(workers: Ref[F, List[GameWorker[F]]]) extends Games[F] {

  def allGames: F[List[Game]] = for {
    workers <- workers.get
    games <- workers.traverse(_.getGame)
  } yield games

  def persistGameAndDeleteFromList(id: UUID, winner: Option[String]): F[Unit] = for {
    workerss <- workers.get
    worker <- workerss.filter(_.id == id).head.pure[F]
    game <- worker.getGame.map(_.toPersist(winner))
    _ <-
      sql"""
        INSERT INTO games (
        id,
        winner,
        player1,
        player2
        ) VALUES (
        ${game.id},
        ${game.winner},
        ${game.player1},
        ${game.player2}
        )
        """.update.run.transact(xa)
    _ <- workers.set(workerss.filter(_.id != id))
  } yield ()


  def cancelGame(id: UUID): F[Unit] = workers.update(_.filter(_.id != id))

  def moveFor(id: UUID, fromTo: (Coordinate, Coordinate)): F[Boolean] = for {
    workerss <- workers.get
    status <- workerss.filter(_.id == id).head.makeMove(fromTo).map(_._2)
    isEnded <- status match {
      case "draw" => persistGameAndDeleteFromList(id, None).map(_ => true)
      case "winBlack" => persistGameAndDeleteFromList(id, Some("black")).map(_ => true)
      case "winWhite" => persistGameAndDeleteFromList(id, Some("white")).map(_ => true)
      case "gameContinue" => false.pure[F]
    }
  } yield isEnded

  def startGame(players: (UUID, UUID)): F[UUID] = {
    val id = UUID.randomUUID()

    for {
      worker <- GameWorker(players, id)
      _ <- workers.update(worker :: _)
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
    workers <- Ref.of[F, List[GameWorker[F]]](Nil)
  } yield new LiveGames[F](xa)(workers)
}

