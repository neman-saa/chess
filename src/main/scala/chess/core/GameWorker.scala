package chess.core

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.kernel.Previous
import cats.syntax.all.*
import chess.domain.chessboard.{Board, Figure, defaultBoard}
import chess.domain.game.Game

import java.util.UUID

class GameWorker[F[_] : Concurrent](game: Ref[F, Game])(val id: UUID) {
  type Coordinate = (Int, Int)
  type GameStatus = String

  def getGameId: F[UUID] = game.get.map(_.id)


  def availableMovesFrom(color: String, current: Coordinate, previousMove: (Figure, (Coordinate, Coordinate)), fieldsWithFigures: (List[Coordinate], List[Coordinate]), board: Board, kingCoordinates: (Coordinate, Coordinate)): F[List[Coordinate]] = {
    def availableMovesFromWeak(color: String, current: Coordinate, previousMove: (Figure, (Coordinate, Coordinate)), fieldsWithFigures: (List[Coordinate], List[Coordinate])): List[Coordinate] = {
      def allMovesByCoordinates(x: Int, y: Int, board: Board, localCurrent: Coordinate, moves: List[Coordinate] = Nil): List[Coordinate] = {

        if (localCurrent._1 + x > 8 || localCurrent._1 + x < 1 || localCurrent._2 + y > 8 || current._2 + y < 1) localCurrent :: moves
        else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.isEmpty) allMovesByCoordinates(x, y, board, (localCurrent._1 + x, localCurrent._2 + y), localCurrent :: moves)
        else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.get.color == color) localCurrent :: moves
        else (localCurrent._1 + x, localCurrent._2 + y) :: localCurrent :: moves
      }

      def weakBishopMoves: List[Coordinate] =
        allMovesByCoordinates(1, 1, board, current) :::
          allMovesByCoordinates(-1, -1, board, current) :::
          allMovesByCoordinates(1, -1, board, current) :::
          allMovesByCoordinates(-1, 1, board, current)

      def weakRookMoves: List[Coordinate] =
        allMovesByCoordinates(0, 1, board, current) :::
          allMovesByCoordinates(0, -1, board, current) :::
          allMovesByCoordinates(1, 0, board, current) :::
          allMovesByCoordinates(-1, 0, board, current)

      def weakQueenMoves: List[Coordinate] =
        rookMoves :::
          bishopMoves

      def weakIsAvailable(coordinate: Coordinate): List[Coordinate] = {
        if (coordinate._1 > 8 || coordinate._1 < 1 || coordinate._2 > 8 || coordinate._2 < 1) Nil
        else if (board.board(current._1 - 1)(current._2 - 1).figure.get.color == color) Nil
        else if (board.board(coordinate._1 - 1)(coordinate._2 - 1).figure.isEmpty) List(coordinate)
        else List(coordinate)
      }

      def weakKingMoves: List[Coordinate] = {
        isAvailable((current._1 + 1, current._2 + 1)) :::
          isAvailable((current._1 + 1, current._2)) :::
          isAvailable((current._1 + 1, current._2 - 1)) :::
          isAvailable((current._1, current._2 - 1)) :::
          isAvailable((current._1 - 1, current._2 - 1)) :::
          isAvailable((current._1 - 1, current._2)) :::
          isAvailable((current._1 - 1, current._2 + 1)) :::
          isAvailable((current._1, current._2 + 1))
      }

      def weakKnightMoves: List[Coordinate] = {
        isAvailable((current._1 + 1, current._2 + 2)) :::
          isAvailable((current._1 + 2, current._2 + 1)) :::
          isAvailable((current._1 + 1, current._2 - 2)) :::
          isAvailable((current._1 - 2, current._2 + 1)) :::
          isAvailable((current._1 - 1, current._2 - 2)) :::
          isAvailable((current._1 - 2, current._2 - 1)) :::
          isAvailable((current._1 + 2, current._2 - 1)) :::
          isAvailable((current._1 - 1, current._2 + 2))
      }

      def weakPawnMoves: List[Coordinate] = {
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

      board.board(current._1 - 1)(current._2 - 1).figure.get match {
        case Figure.ROOK1 => weakRookMoves
        case Figure.ROOK2 => weakRookMoves
        case Figure.KNIGHT => weakKnightMoves
        case Figure.BISHOP => weakBishopMoves
        case Figure.QUEEN => weakQueenMoves
        case Figure.KING => weakKingMoves
        case Figure.PAWN => weakPawnMoves
      }
    }

    def allMovesByCoordinates(x: Int, y: Int, board: Board, localCurrent: Coordinate, moves: List[Coordinate] = Nil): List[Coordinate] = {

      if (localCurrent._1 + x > 8 || localCurrent._1 + x < 1 || localCurrent._2 + y > 8 || current._2 + y < 1) localCurrent :: moves
      else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.isEmpty) allMovesByCoordinates(x, y, board, (localCurrent._1 + x, localCurrent._2 + y), localCurrent :: moves)
      else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.get.color == color) localCurrent :: moves
      else (localCurrent._1 + x, localCurrent._2 + y) :: localCurrent :: moves
    }

    def bishopMoves: List[Coordinate] =
      allMovesByCoordinates(1, 1, board, current) :::
        allMovesByCoordinates(-1, -1, board, current) :::
        allMovesByCoordinates(1, -1, board, current) :::
        allMovesByCoordinates(-1, 1, board, current)

    def rookMoves: List[Coordinate] =
      allMovesByCoordinates(0, 1, board, current) :::
        allMovesByCoordinates(0, -1, board, current) :::
        allMovesByCoordinates(1, 0, board, current) :::
        allMovesByCoordinates(-1, 0, board, current)

    def queenMoves: List[Coordinate] =
      rookMoves :::
        bishopMoves

    def isAvailable(coordinate: Coordinate): List[Coordinate] = {
      if (coordinate._1 > 8 || coordinate._1 < 1 || coordinate._2 > 8 || coordinate._2 < 1) Nil
      else if (board.board(current._1 - 1)(current._2 - 1).figure.get.color == color) Nil
      else if (board.board(coordinate._1 - 1)(coordinate._2 - 1).figure.isEmpty) List(coordinate)
      else List(coordinate)
    }

    def kingMoves: List[Coordinate] = {
      isAvailable((current._1 + 1, current._2 + 1)) :::
        isAvailable((current._1 + 1, current._2)) :::
        isAvailable((current._1 + 1, current._2 - 1)) :::
        isAvailable((current._1, current._2 - 1)) :::
        isAvailable((current._1 - 1, current._2 - 1)) :::
        isAvailable((current._1 - 1, current._2)) :::
        isAvailable((current._1 - 1, current._2 + 1)) :::
        isAvailable((current._1, current._2 + 1))
    }

    def knightMoves: List[Coordinate] = {
      isAvailable((current._1 + 1, current._2 + 2)) :::
        isAvailable((current._1 + 2, current._2 + 1)) :::
        isAvailable((current._1 + 1, current._2 - 2)) :::
        isAvailable((current._1 - 2, current._2 + 1)) :::
        isAvailable((current._1 - 1, current._2 - 2)) :::
        isAvailable((current._1 - 2, current._2 - 1)) :::
        isAvailable((current._1 + 2, current._2 - 1)) :::
        isAvailable((current._1 - 1, current._2 + 2))
    }

    def pawnMoves: List[Coordinate] = {
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
      case "white" => !fieldsWithFigures._1.flatMap(a => availableMovesFromWeak(turn, a, previousMove, fieldsWithFigures)).contains(kingCoordinates._2)
      case "black" => !fieldsWithFigures._2.flatMap(a => availableMovesFromWeak(turn, a, previousMove, fieldsWithFigures)).contains(kingCoordinates._1)
    }


    val a = board.board(current._1 - 1)(current._2 - 1).figure.get.figure match {
      case Figure.ROOK1 => rookMoves
      case Figure.ROOK2 => rookMoves
      case Figure.KNIGHT => knightMoves
      case Figure.BISHOP => bishopMoves
      case Figure.QUEEN => queenMoves
      case Figure.KING => kingMoves
      case Figure.PAWN => pawnMoves
    }
    a.pure[F]
  }

  def getGame: F[Game] = game.get

  def makeMove(fromTo: (Coordinate, Coordinate)): F[(Game, GameStatus)] = ???

}

object GameWorker {
  def apply[F[_] : Concurrent](players: (UUID, UUID), id: UUID): F[GameWorker[F]] = for {
    game <- Ref.of[F, Game](Game(defaultBoard, players, id, 0))
  } yield new GameWorker[F](game)(id)
}


