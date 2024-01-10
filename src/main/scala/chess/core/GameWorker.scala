package chess.core

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.kernel.Previous
import chess.domain.chessboard.{Board, defaultBoard}
import chess.domain.game.Game
import cats.syntax.all.*

import java.util.UUID

class GameWorker[F[_]: Concurrent](game: Ref[F, Game])(val id: UUID) {

  def getGameId: F[UUID] = game.get.map(_.id)

  def availableMovesFrom(color: String, current: Coordinate, previousMove: (Figure, (Coordinate, Coordinate))): F[List[(Coordinate, Coordinate)]] = {

    def allMovesByCoordinates(x: Int, y: Int, board: Board, moves: List[Coordinate] = Nil): List[Coordinate] = {

      if (current._1 + x > 8 || current._1 + x < 1 || current._2 + y > 8 || current._2 + y < 1) current :: moves
      else if (board.board(current._1 - 1 + x)(current._2 - 1 + y).figure.isEmpty) allMovesByCoordinates(x, y, board, color, (current._1 + x, current._2 + y), current :: moves)
      else if (board.board(current._1 - 1 + x)(current._2 - 1 + y).figure.get.color == color) current :: moves
      else (current._1 + x, current._2 + y) :: current :: moves
    }

    def bishopMoves(color: String, board: Board, current: Coordinate): List[Coordinate] =
      allMovesByCoordinates(1, 1, board, color, current) :::
      allMovesByCoordinates(-1, -1, board, color, current) :::
      allMovesByCoordinates(1, -1, board, color, current) :::
      allMovesByCoordinates(-1, 1, board, color, current)

    def rookMoves(color: String, board: Board, current: Coordinate): List[Coordinate] =
      allMovesByCoordinates(0, 1, board, color, current) :::
      allMovesByCoordinates(0, -1, board, color, current) :::
      allMovesByCoordinates(1, 0, board, color, current) :::
      allMovesByCoordinates(-1, 0, board, color, current)

    def queenMoves(color: String, board: Board, current: Coordinate): List[Coordinate] =
      rookMoves(color, board, current) :::
      bishopMoves(color, board, current)

    def pawnMoves(board: Board, color: String, current: Coordinate) = {
      color match {
        case "white" =>
          val forward: List[Coordinate] = if (current._2 == 2) {
            if (board.board(current._1 - 1)(current._2).figure.isDefined) Nil
            else if (board.board(current._1 - 1)(current._2 + 1).figure.isDefined) List((current._1, current._2 + 1))
            else List((current._1, current._2 + 1), (current._1, current._2 + 2))
          }
          else if (board.board(current._1 - 1)(current._2).figure.isDefined) Nil
          else List((current._1, current._2 + 1))

      }
    }

    def isPositionLegal(board: Board, turn: String): Boolean = turn match {
      case "white" =>
    }
    ???
  }

  def getGame: F[Game] = game.get

  def makeMove(fromTo: (Coordinate, Coordinate)): F[(Game, GameStatus)] = ???

}

object GameWorker {
  def apply[F[_]: Concurrent](players: (UUID, UUID), id: UUID): F[GameWorker[F]] = for {
    game <- Ref.of[F, Game](Game(defaultBoard, players, id, 0))
  } yield new GameWorker[F](game)(id)
}


