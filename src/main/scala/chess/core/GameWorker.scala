package chess.core

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.syntax.*
import cats.instances.list.*
import cats.kernel.Previous
import cats.syntax.all.*
import chess.domain.chessboard.{Board, Field, Figure, defaultBoard}
import chess.domain.game.{Game, GameInfo}

import java.util.UUID

class GameWorker[F[_] : Concurrent](game: Ref[F, GameInfo])(val id: UUID) {
  type Coordinate = (Int, Int)
  type GameStatus = String

  def getGameId: F[UUID] = id.pure[F]


  def availableMovesFrom(gameInfo: GameInfo, from: Coordinate): F[List[Coordinate]] = {

    def availableMovesFromWeak(gameInfo: GameInfo, from: Coordinate): List[Coordinate] = {
      val board = gameInfo.board
      val current = from
      val turn = gameInfo.turn
      val previousMove = gameInfo.previousMove

      def allMovesByCoordinatesWeak(x: Int, y: Int, board: Board, localCurrent: Coordinate, color: String, moves: List[Coordinate] = Nil): List[Coordinate] = {
        if (localCurrent._1 + x > 8 || localCurrent._1 + x < 1 || localCurrent._2 + y > 8 || localCurrent._2 + y < 1) localCurrent :: moves
        else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.isEmpty) allMovesByCoordinatesWeak(x, y, board, (localCurrent._1 + x, localCurrent._2 + y), color, localCurrent :: moves)
        else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.get.color == color) localCurrent :: moves
        else (localCurrent._1 + x, localCurrent._2 + y) :: localCurrent :: moves
      }

      def weakBishopMoves: List[Coordinate] =
        allMovesByCoordinatesWeak(1, 1, board, current, turn) :::
          allMovesByCoordinatesWeak(-1, -1, board, current, turn) :::
          allMovesByCoordinatesWeak(1, -1, board, current, turn) :::
          allMovesByCoordinatesWeak(-1, 1, board, current, turn)

      def weakRookMoves: List[Coordinate] =
        allMovesByCoordinatesWeak(0, 1, board, current, turn) :::
          allMovesByCoordinatesWeak(0, -1, board, current, turn) :::
          allMovesByCoordinatesWeak(1, 0, board, current, turn) :::
          allMovesByCoordinatesWeak(-1, 0, board, current, turn)

      def weakQueenMoves: List[Coordinate] =
        weakRookMoves :::
          weakBishopMoves

      def isAvailable(coordinate: Coordinate, color: String): List[Coordinate] = {
        if (coordinate._1 > 8 || coordinate._1 < 1 || coordinate._2 > 8 || coordinate._2 < 1) Nil
        else if (board.board(current._1 - 1)(current._2 - 1).figure.get.color == color) Nil
        else if (board.board(coordinate._1 - 1)(coordinate._2 - 1).figure.isEmpty) List(coordinate)
        else List(coordinate)
      }

      def weakKingMoves: List[Coordinate] = {
        isAvailable((current._1 + 1, current._2 + 1), turn) :::
          isAvailable((current._1 + 1, current._2), turn) :::
          isAvailable((current._1 + 1, current._2 - 1), turn) :::
          isAvailable((current._1, current._2 - 1), turn) :::
          isAvailable((current._1 - 1, current._2 - 1), turn) :::
          isAvailable((current._1 - 1, current._2), turn) :::
          isAvailable((current._1 - 1, current._2 + 1), turn) :::
          isAvailable((current._1, current._2 + 1), turn)
      }

      def weakKnightMoves: List[Coordinate] = {
        isAvailable((current._1 + 1, current._2 + 2), turn) :::
          isAvailable((current._1 + 2, current._2 + 1), turn) :::
          isAvailable((current._1 + 1, current._2 - 2), turn) :::
          isAvailable((current._1 - 2, current._2 + 1), turn) :::
          isAvailable((current._1 - 1, current._2 - 2), turn) :::
          isAvailable((current._1 - 2, current._2 - 1), turn) :::
          isAvailable((current._1 + 2, current._2 - 1), turn) :::
          isAvailable((current._1 - 1, current._2 + 2), turn)
      }

      def weakPawnMoves: List[Coordinate] = turn match {
        case "white" =>
          def enPassant: List[Coordinate] = previousMove match {
            case Some(figure, ((x1, y1), (x2, y2))) if figure == Figure.PAWN && (x1 == current._1 + 1 && y1 == current._2 + 2) && (y2 == current._2) => List((current._1 + 1, 6))
            case Some(figure, ((x1, y1), (x2, y2))) if figure == Figure.PAWN && (x1 == current._1 - 1 && y1 == current._2 + 2) && (y2 == current._2) => List((current._1 - 1, 6))
            case _ => Nil
          }

          def simpleMove: List[Coordinate] = {
            if (current._2 == 2) {
              if (board.board(current._1 - 1)(current._2).figure.isEmpty)
                List((current._1, current._2 + 1))
              else Nil
            }
              ::: {
              if (board.board(current._1 - 1)(current._2).figure.isEmpty && board.board(current._1 - 1)(current._2 + 1).figure.isEmpty)
                List((current._1, current._2 + 2))
              else Nil
            }
            else if (board.board(current._1 - 1)(current._2).figure.isEmpty) List((current._1, current._2 + 1))
            else Nil
          }

          def take: List[Coordinate] = {
            {
              if (board.board(current._1)(current._2).figure.isEmpty) Nil
              else if (board.board(current._1)(current._2).figure.get.color == turn) Nil
              else List((current._1 + 1, current._2 + 1))
            } ::: {
              if (board.board(current._1 - 2)(current._2).figure.isEmpty) Nil
              else if (board.board(current._1 - 2)(current._2).figure.get.color == turn) Nil
              else List((current._1 - 1, current._2 + 1))
            }
          }

          enPassant ::: simpleMove ::: take

        case "black" =>
          def enPassant: List[Coordinate] = previousMove match {
            case Some(figure, ((x1, y1), (x2, y2))) if figure == Figure.PAWN && (x1 == current._1 + 1 && y1 == current._2 -2) && (y2 == current._2) => List((current._1 + 1, 3))
            case Some(figure, ((x1, y1), (x2, y2))) if figure == Figure.PAWN && (x1 == current._1 - 1 && y1 == current._2 -2) && (y2 == current._2 ) => List((current._1 - 1, 3))
            case _ => Nil
          }

          def simpleMove: List[Coordinate] = {
            if (current._2 == 7) {
              if (board.board(current._1 - 1)(current._2 - 2).figure.isEmpty) List((current._1, current._2 - 1))
              else Nil
            } ::: {
              if (board.board(current._1 - 1)(current._2 - 2).figure.isEmpty && board.board(current._1 - 1)(current._2 - 3).figure.isEmpty) List((current._1, current._2 - 2))
              else Nil
            }
            else if (board.board(current._1 - 1)(current._2 - 2).figure.isEmpty) List((current._1, current._2 - 1))
            else Nil
          }

          def take: List[Coordinate] = {
            {
              if (board.board(current._1)(current._2 - 2).figure.isEmpty) Nil
              else if (board.board(current._1)(current._2 - 2).figure.get.color == turn) Nil
              else List((current._1 + 1, current._2 - 1))
            } ::: {
              if (board.board(current._1 - 2)(current._2 - 2).figure.isEmpty) Nil
              else if (board.board(current._1 - 2)(current._2 - 2).figure.get.color == turn) Nil
              else List((current._1 - 1, current._2 - 1))
            }
          }

          enPassant ::: simpleMove ::: take
      }

      //board.board(current._1 - 1)(current._2 - 1).figure.get.color match {
      //case c if c == turn =>
      board.board(current._1 - 1)(current._2 - 1).figure.get.figure match {
        case Figure.ROOK1 => weakRookMoves
        case Figure.ROOK2 => weakRookMoves
        case Figure.KNIGHT => weakKnightMoves
        case Figure.BISHOP => weakBishopMoves
        case Figure.QUEEN => weakQueenMoves
        case Figure.KING => weakKingMoves
        case Figure.PAWN => weakPawnMoves
      }
    }
    // case _ => Nil
    //}

    def isPositionLegal(gameInfo: GameInfo): Boolean = gameInfo.turn match {
      case "white" => !gameInfo.fieldsWithFigures._1.flatMap(a => availableMovesFromWeak(gameInfo, a)).contains(gameInfo.kingCoordinates._2)
      case "black" => !gameInfo.fieldsWithFigures._2.flatMap(a => availableMovesFromWeak(gameInfo, a)).contains(gameInfo.kingCoordinates._1)
    }

    def makeMovePrediction(gameInfo: GameInfo, fromTo: (Coordinate, Coordinate)): Board = {
      val board = gameInfo.board.board
      val columnToUpdate1 = board(fromTo._1._1 - 1)
      val updated1 = columnToUpdate1.updated(fromTo._1._2 - 1, Field(fromTo._1, None))
      val columnToUpdate2 = board(fromTo._2._1 - 1)
      val updated2 = columnToUpdate2.updated(fromTo._2._2 - 1, Field(fromTo._2, board(fromTo._1._1 - 1)(fromTo._1._2 - 1).figure))
      Board(board.updated(fromTo._1._1 - 1, updated1).updated(fromTo._2._1 - 1, updated2))
    }

    ???
  }

  def getGame: F[GameInfo] = game.get

  def isEnded(gameInfo: GameInfo): F[Boolean] = gameInfo.turn.match {
    case "white" => gameInfo.fieldsWithFigures._1.flatTraverse(availableMovesFrom(gameInfo, _)).map(_.isEmpty)
    case "black" => gameInfo.fieldsWithFigures._2.flatTraverse(availableMovesFrom(gameInfo, _)).map(_.isEmpty)
  }


  def makeMove(fromTo: (Coordinate, Coordinate)): F[GameStatus] = for {
    tr <- game.updateAndGet(g => g.copy(
      moves = g.moves + 1,
      previousMove = Some(g.board.board(fromTo._1._1 - 1)(fromTo._1._2 - 1).figure.get.figure, fromTo),
      kingCoordinates = {
        val figure = g.board.board(fromTo._1._1 - 1)(fromTo._1._2 - 1).figure
        if (figure.get.figure == Figure.KING && figure.get.color == "white") (fromTo._2, g.kingCoordinates._2)
        else if (figure.get.figure == Figure.KING && figure.get.color == "black") (g.kingCoordinates._1, fromTo._2)
        else g.kingCoordinates
      },
      isCastleAvailable = g.isCastleAvailable, //!!!!!!!!! TODO
      fieldsWithFigures = {
        val color = g.board.board(fromTo._1._1)(fromTo._1._2).figure.get.color

        color match {
          case "white" => (g.fieldsWithFigures._1.filter(_ != fromTo._1), g.fieldsWithFigures._2.filter(_ != fromTo._2))
          case "black" => (g.fieldsWithFigures._1.filter(_ != fromTo._2), g.fieldsWithFigures._2.filter(_ != fromTo._1))
        }
      }
    ))

  } yield "TODO"
}


object GameWorker {
  def apply[F[_] : Concurrent](players: (UUID, UUID), id: UUID): F[GameWorker[F]] = for {
    game <- Ref.of[F, GameInfo](GameInfo(defaultBoard))
  } yield new GameWorker[F](game)(id)
}


