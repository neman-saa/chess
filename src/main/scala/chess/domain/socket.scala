package chess.domain
import java.util.UUID

import chess.domain.chessboard.Figure
import chess.domain.game.*
import chess.domain.game.Move.*
import io.circe.generic.semiauto.*

object socket {

  sealed trait InputMessage
  object InputMessage {

    case class MakeMove(move: Move, userId: UUID, roomId: UUID) extends InputMessage
    case class CancelGame(id: UUID)                             extends InputMessage
    case class GiveUp(userId: UUID, roomId: UUID)               extends InputMessage

    case class BadRequest(userId: UUID)                                           extends InputMessage
    case class AllMovesFrom(playerId: UUID, roomId: UUID, coordinate: Coordinate) extends InputMessage
    def parse(userId: UUID, text: String, roomId: UUID): InputMessage = text.split(";").toList match {
      case "giveUp" :: Nil => GiveUp(userId, roomId)
      case "simpleMove" :: figure :: x1 :: y1 :: x2 :: y2 :: Nil =>
        MakeMove(SimpleMove(Figure.valueOf(figure), ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))), userId, roomId)
      case "enPassant" :: x1 :: y1 :: x2 :: y2 :: Nil =>
        MakeMove(EnPassant(((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))), userId, roomId)
      case "promotion" :: promotesTo :: x1 :: y1 :: x2 :: y2 :: Nil =>
        MakeMove(Promotion(((x1.toInt, y1.toInt), (x2.toInt, y2.toInt)), Figure.valueOf(promotesTo)), userId, roomId)
      case "castle" :: right :: x1 :: y1 :: x2 :: y2 :: Nil =>
        MakeMove(Castle(right.toBoolean, ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))), userId, roomId)
      case "availableMovesFrom" :: x :: y :: Nil => AllMovesFrom(userId, roomId, (x.toInt, y.toInt))
      case _                                     => BadRequest(userId)
    }
  }

  case class MakeMove()
}
