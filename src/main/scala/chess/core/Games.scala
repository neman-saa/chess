package chess.core

import java.util.UUID

import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.effect.Ref
import cats.instances.list.*
import cats.syntax.all.*
import cats.Traverse
import chess.core.GameWorker.*
import chess.domain.chessboard.defaultBoard
import chess.domain.game.*
import chess.domain.game.GameStatus.*
import chess.domain.socket.InputMessage
import chess.domain.socket.InputMessage.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger

type Coordinate = (Int, Int)
type GameStatus = String
type RoomId     = UUID
type Player1    = UUID
type Player2    = UUID
type Turn       = String
trait Games[F[_]] {
  def allGames: F[List[RoomId]]
  def persistGameAndDeleteFromList(id: RoomId, winner: Option[String]): F[Unit]
  def cancelGame(id: RoomId): F[Unit]
  def endGame(loser: UUID, id: RoomId): F[Unit]
  def moveFor(id: RoomId, move: Move, player: UUID): F[Unit]
  def availableMovesFrom(player: UUID, roomId: RoomId, coordinate: Coordinate): F[Unit]
  def startGame(roomId: RoomId, players: (Player1, Player2)): F[RoomId]
  def getGameById(id: RoomId): F[Option[GameForPersistence]]
  def startConnectingGame(id: RoomId, player1: Player1, player2: Player2): F[Unit]
  def connect(roomId: RoomId, player: UUID): F[Either[String, Unit]]
  def removeConnectingGame(id: RoomId): F[Unit]
  def processMessage(message: InputMessage): F[Unit]
}

class LiveGames[F[_]: Concurrent: Logger](xa: Transactor[F])(
    playersAreConnecting: Ref[F, Map[RoomId, (List[UUID], Int)]],
    games: Ref[F, Map[RoomId, ((Player1, Player2), GameWorker[F], Turn)]],
    sessions: Sessions[F],
    users: Users[F]
) extends Games[F] {

  override def allGames: F[List[RoomId]] = games.get.map(map => map.keys.toList)

  override def persistGameAndDeleteFromList(id: RoomId, winner: Option[String]): F[Unit] = for {
    workers <- games.get
    worker  <- workers(id).pure[F]
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

  override def endGame(loser: UUID, id: RoomId): F[Unit] = for {
    players <- games.get.map(_.get(id).map(_._1))
    winner = players.get match {
      case players if players._1 == loser => "white"
      case players if players._2 == loser => "black"
    }
    _ <-
      players.traverse(players =>
        sessions
          .offer(players._1, s"Game ended, $winner won.")
          .flatMap(_ => sessions.offer(players._2, s"Game, ended, $winner won."))
      )
    _ <- persistGameAndDeleteFromList(id, Some(winner))
  } yield ()

  override def cancelGame(id: RoomId): F[Unit] =
    games
      .update(_ - id)
      .flatMap(_ => playersAreConnecting.update(_ - id))

  override def moveFor(id: RoomId, move: Move, player: UUID): F[Unit] = for {
    workers       <- games.get
    mbWorkerTuple <- workers.get(id).pure[F]
    _ <- mbWorkerTuple match {
      case None => sessions.offer(player, "Game has not started yet")
      case Some(workerTuple) =>
        val continue = workerTuple._3 == { if (player == workerTuple._1._1) "white" else "black" }
        if (continue)
          workerTuple._2.makeMove(move).flatMap {
            case GameContinuing =>
              val secondPlayer = if (workerTuple._1._1 == player) workerTuple._1._2 else workerTuple._1._1
              sessions.offer(player, "game continuing") *> sessions
                .offer(secondPlayer, s"game continuing; $move")
            case NotAValidMove => sessions.offer(player, "you cannot go there")
            case status =>
              for {
                players <- games.get.map(map => map(id)._1)
                _       <- persistGameAndDeleteFromList(id, Some(status.toString))
                _       <- sessions.offer(players._1, s"$status; $move")
                _       <- sessions.offer(players._2, s"$status; $move")
                _       <- sessions.update(players._1, None)
                _       <- sessions.update(players._2, None)
              } yield ()
          }
        else sessions.offer(player, "Not your turn")
    }
  } yield ()

  override def availableMovesFrom(player: UUID, roomId: RoomId, coordinate: Coordinate): F[Unit] = for {
    players <- games.get.map(_.get(roomId))
    res <- players match {
      case None => Left("Game has not started yet.").pure[F]
      case Some(((p1, p2), worker, turn)) =>
        if ({ if (p1 == player) "white" else "black" } == turn) {
          worker.availableMovesFrom(coordinate).map(Right(_))
        } else Left("Not your turn.").pure[F]
    }
    _ <- sessions.offer(player, res.toString)
  } yield ()

  override def startGame(roomId: RoomId, players: (Player1, Player2)): F[RoomId] = {
    for {
      worker <- GameWorker(roomId)
      _      <- sessions.offer(players._1, "Game started, your turn.")
      _      <- sessions.offer(players._2, "Game started.")
      _      <- games.update(map => map + (roomId -> (players, worker, "white")))
    } yield roomId
  }
  override def getGameById(id: RoomId): F[Option[GameForPersistence]] =
    sql"""
         SELECT * FROM games WHERE games.id = $id
         """
      .query[GameForPersistence]
      .option
      .transact(xa)
  override def startConnectingGame(id: RoomId, player1: Player1, player2: Player2): F[Unit] =
    playersAreConnecting.update(connectingGames => connectingGames + (id -> (List(player1, player2), 2)))
  override def removeConnectingGame(id: RoomId): F[Unit] = playersAreConnecting.update(_.filter(_._1 != id))

  override def connect(roomId: RoomId, player: UUID): F[Either[String, Unit]] = for {
    connectingGames <- playersAreConnecting.get
    mbRoom = connectingGames.get(roomId)
    result <- mbRoom match {
      case Some(room) =>
        room._1.length match {
          case 1 if room._1.contains(player) =>
            startGame(roomId, (room._1.head, room._1.tail.head))
              .flatMap(_ => removeConnectingGame(roomId))
              .map(_ => Right {})
          case 2 if room._1.contains(player) =>
            playersAreConnecting.update(map => map + (roomId -> (room._1, 1))).map(_ => Right {})
          case _ => Left("You can not connect to this game").pure[F]
        }
      case None => Left("No such game").pure[F]
    }
  } yield result

  override def processMessage(message: InputMessage): F[Unit] = message match {
    case GiveUp(userId, roomId)                     => endGame(userId, roomId)
    case MakeMove(move, userId, roomId)             => moveFor(roomId, move, userId)
    case CancelGame(id)                             => cancelGame(id)
    case BadRequest(userId)                         => sessions.offer(userId, "Incorrect input")
    case AllMovesFrom(playerId, roomId, coordinate) => availableMovesFrom(playerId, roomId, coordinate)
  }
}

object LiveGames {

  given gamesRead: Read[GameForPersistence] = Read[(UUID, Option[String], UUID, UUID)].map {
    case (id: UUID, winner: Option[String], player1: UUID, player2: UUID) =>
      GameForPersistence(id, winner, player1, player2)
  }

  def apply[F[_]: Concurrent: Logger](xa: Transactor[F])(sessions: Sessions[F], users: Users[F]): F[LiveGames[F]] =
    for {
      workers    <- Ref.of[F, Map[UUID, ((UUID, UUID), GameWorker[F], Turn)]](Map())
      connecting <- Ref.of[F, Map[UUID, (List[UUID], Int)]](Map.empty)
    } yield new LiveGames[F](xa)(connecting, workers, sessions, users)
}
