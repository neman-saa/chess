package chess.domain

import chess.domain.chessboard.{Board, Figure, FigureWithColor}
import chess.domain.user.User

import java.util.UUID

object game {

  type Coordinate = (Int, Int)

  case class Game(
                   board: Board,
                   players: (UUID, UUID),
                   id: UUID,
                   moves: Int,
                   previousMove: Option[(Figure, (Coordinate, Coordinate))] = None,
                   isCastleAvailable: (Boolean, Boolean, Boolean, Boolean) = (true, true, true, true),
                   kingCoordinates: (Coordinate, Coordinate) = ((5,1) ,(5,8)),
                   fieldsWithFigures: (List[Coordinate], List[Coordinate]) = (
                     List((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (3, 2), (4, 1), (4, 2), (5, 1), (5, 2), (6, 1), (6, 2), (7, 1), (7, 2), (8, 1), (8, 2)),
                     List((1, 7), (1, 8), (2, 7), (2, 8), (3, 7), (3, 8), (4, 7), (4, 8), (5, 7), (5, 8), (6, 7), (6, 8), (7, 7), (7, 8), (8, 7), (8, 8))
                   )) {
    def toPersist(winner: Option[String]): GameForPersistence = GameForPersistence(id, winner, players._1, players._2)
  }

  case class GameForPersistence(id: UUID, winner: Option[String], player1: UUID, player2: UUID)
}

