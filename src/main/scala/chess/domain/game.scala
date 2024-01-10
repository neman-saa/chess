package chess.domain

import chess.domain.chessboard.{Board, FigureWithColor}
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
                   isCastleAvailable: (Boolean, Boolean, Boolean, Boolean),
                   fieldsWithFigures: (List[Coordinate], List[Coordinate])) {
    def toPersist(winner: Option[String]): GameForPersistence = GameForPersistence(id, winner, players._1, players._2)
  }
  
  case class GameForPersistence(id: UUID, winner: Option[String], player1: UUID, player2: UUID)
  
}
