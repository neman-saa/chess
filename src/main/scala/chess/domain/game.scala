package chess.domain

import chess.domain.chessboard.{Board, Figure, FigureWithColor}
import chess.domain.user.User

import java.util.UUID

object game {

  type Coordinate = (Int, Int)

  trait Move(val figure: Figure, val fromTo: (Coordinate, Coordinate))

  case class SimpleMove(figuree: Figure, fromToo: (Coordinate, Coordinate)) extends Move(figuree, fromToo)

  case class EnPassant(fromToo: (Coordinate, Coordinate)) extends Move(Figure.PAWN, fromToo)

  case class Castle(right: Boolean, fromToo: (Coordinate, Coordinate)) extends Move(Figure.KING, fromToo)

  case class Promotion(fromToo: (Coordinate, Coordinate), promotesTo: Figure) extends Move(Figure.PAWN, fromToo)
  case class GameInfo(
                   board: Board,
                   moves: Int = 0,
                   previousMove: Option[Move] = None,
                   isCastleAvailable: List[Boolean] = List(true, true, true, true),
                   kingCoordinates: (Coordinate, Coordinate) = ((5,1), (5,8)),
                   turn: String = "white")
  case class Game(players: (UUID, UUID), id: UUID, gameInfo: GameInfo) {
    def toPersist(winner: Option[String]): GameForPersistence = GameForPersistence(id, winner, players._1, players._2)
  }

  case class GameForPersistence(id: UUID, winner: Option[String], player1: UUID, player2: UUID)
}

