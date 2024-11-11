package chess.domain

import java.util.UUID

import chess.domain.chessboard._
import chess.domain.game.Move.*
import chess.domain.user.User

object game {

  type Coordinate = (Int, Int)

  sealed trait GameStatus

  object GameStatus {
    object WhiteWin extends GameStatus

    object BlackWin extends GameStatus

    object GameContinuing extends GameStatus

    object NotAValidMove extends GameStatus

    object Draw extends GameStatus

  }
  trait Move(val figure: Figure, val fromTo: (Coordinate, Coordinate)) {
    def toString: String
  }

  object Move {
    case class SimpleMove(figuree: Figure, fromToo: (Coordinate, Coordinate)) extends Move(figuree, fromToo) {
      override def toString: String =
        s"simpleMove;${figure.toString.trim};${fromToo._1._1};${fromToo._1._2};${fromToo._2._1};${fromToo._2._2}"
    }

    case class EnPassant(fromToo: (Coordinate, Coordinate)) extends Move(Figure.PAWN, fromToo) {
      override def toString: String = s"enPassant;${fromToo._1._1};${fromToo._1._2};${fromToo._2._1};${fromToo._2._2}"
    }

    case class Castle(right: Boolean, fromToo: (Coordinate, Coordinate)) extends Move(Figure.KING, fromToo) {
      override def toString: String =
        s"castle;$right;${fromToo._1._1};${fromToo._1._2};${fromToo._2._1};${fromToo._2._2}"
    }

    case class Promotion(fromToo: (Coordinate, Coordinate), promotesTo: Figure) extends Move(Figure.PAWN, fromToo) {
      override def toString: String = s"promotion;${fromToo._1._1};${fromToo._1._2};${fromToo._2._1};${fromToo._2._2}"
    }

  }
  case class GameInfo(
      board: Board,
      moves: Int = 0,
      previousMove: Option[Move] = None,
      isCastleAvailable: List[Boolean] = List(true, true, true, true),
      kingCoordinates: (Coordinate, Coordinate) = ((5, 1), (5, 8)),
      turn: String = "white",
      fiftyMovesRole: Int = 0
  )
  case class Game(players: (UUID, UUID), id: UUID, gameInfo: GameInfo) {
    def toPersist(winner: Option[String]): GameForPersistence = GameForPersistence(id, winner, players._1, players._2)
  }

  case class GameForPersistence(id: UUID, winner: Option[String], player1: UUID, player2: UUID)
}
