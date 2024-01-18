package chess.core

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.syntax.*
import cats.instances.list.*
import cats.kernel.Previous
import cats.syntax.all.*
import chess.domain.chessboard.Figure.ROOK1
import chess.domain.chessboard.{Board, Field, Figure, FigureWithColor, defaultBoard}
import chess.domain.game.*

import java.util.UUID

class GameWorker[F[_] : Concurrent](game: Ref[F, GameInfo])(val id: UUID) {
  type Coordinate = (Int, Int)
  type GameStatus = String


  def getGameId: F[UUID] = id.pure[F]

  def availableMovesFrom(gameInfo: GameInfo, from: Coordinate): F[List[Move]] = {

    val isCastleAvailable = gameInfo.isCastleAvailable
    val board = gameInfo.board.board
    val fieldsWithFigures = gameInfo.fieldsWithFigures

    def castle(turn: String, right: Boolean): List[Move] = (turn, right) match {
      case ("white", true) if gameInfo.isCastleAvailable(1) => if(
        isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (7, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (5, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (6, 1)))))
      ) List(Castle(true, ((5, 1), (7, 1)))) else Nil
      case ("black", true) if gameInfo.isCastleAvailable(3) => if (
        isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (7, 8))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (5, 8))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (6, 8)))))
      ) List(Castle(true, ((5, 8), (7, 8)))) else Nil
      case ("black", false) if gameInfo.isCastleAvailable(2) => if (
        isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (5, 8))))) &&
        isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (4, 8))))) &&
        isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (3, 8)))))
      ) List(Castle(false, ((5, 8), (3, 8)))) else Nil
      case ("white", false) if gameInfo.isCastleAvailable.head => if (
        isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (5, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (4, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (3, 1)))))
      ) List(Castle(false, ((5, 1), (3, 1)))) else Nil
      case _ => Nil
    }

    def promotion: List[Move] = gameInfo.turn match {
      case "white" =>
        val movesFromToCoordinates =
        fieldsWithFigures._1.filter(coordinate => board(coordinate._1 - 1)(coordinate._2 - 1) match {
          case Field(_, Some(FigureWithColor(Figure.PAWN, _))) => true
          case _ => false
        }).filter(coordinate => board(coordinate._1 - 1)(coordinate._2) match {
          case Field(_, None) => true
          case _ => false
        }).filter(_._2 == 7).map(x => (x, (x._1, x._2 + 1)))
        for {
          fromTo <- movesFromToCoordinates
          figure <- List(Figure.KNIGHT, Figure.BISHOP, Figure.NoCastleROOK, Figure.QUEEN)
        } yield Promotion(fromTo, figure)
      case "black" =>
        val movesFromToCoordinates =
          fieldsWithFigures._2.filter(coordinate => board(coordinate._1 - 1)(coordinate._2 - 1) match {
            case Field(_, Some(FigureWithColor(Figure.PAWN, _))) => true
            case _ => false
          }).filter(coordinate => board(coordinate._1 - 1)(coordinate._2) match {
            case Field(_, None) => true
            case _ => false
          }).filter(_._2 == 2).map(x => (x, (x._1, x._2 - 1)))
        for {
          fromTo <- movesFromToCoordinates
          figure <- List(Figure.KNIGHT, Figure.BISHOP, Figure.NoCastleROOK, Figure.QUEEN)
        } yield Promotion(fromTo, figure)
    }

    (castle(gameInfo.turn, true) |+| castle(gameInfo.turn, false) |+| promotion |+| availableMovesFromWeak(gameInfo, from)).filter(move => isPositionLegal(makeMovePrediction(gameInfo, move))).pure[F]
  }

  def availableMovesFromWeak(gameInfo: GameInfo, from: Coordinate): List[Move] = {
    val board = gameInfo.board
    val current = from
    val turn = gameInfo.turn
    val previousMove = gameInfo.previousMove

    def allMovesByCoordinatesWeak(x: Int, y: Int, board: Board, localCurrent: Coordinate, color: String, moves: List[Move] = Nil, figure: Figure): List[Move] =
      if (localCurrent._1 + x > 8 || localCurrent._1 + x < 1 || localCurrent._2 + y > 8 || localCurrent._2 + y < 1) moves
      else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.isEmpty)
        allMovesByCoordinatesWeak(x, y, board, (localCurrent._1 + x, localCurrent._2 + y), color, SimpleMove(figure, (current, (localCurrent._1 + x, localCurrent._2 + y))) :: moves, figure)
      else if (board.board(localCurrent._1 - 1 + x)(localCurrent._2 - 1 + y).figure.get.color == color) moves
      else SimpleMove(figure, (current, (localCurrent._1 + x, localCurrent._2 + y))) :: moves

    def weakBishopMoves: List[Move] =
      allMovesByCoordinatesWeak(1, 1, board, current, turn, Nil, Figure.BISHOP) |+|
        allMovesByCoordinatesWeak(-1, -1, board, current, turn, Nil, Figure.BISHOP) |+|
        allMovesByCoordinatesWeak(1, -1, board, current, turn, Nil, Figure.BISHOP) |+|
        allMovesByCoordinatesWeak(-1, 1, board, current, turn, Nil, Figure.BISHOP)

    def weakRookMoves(rook: Figure): List[Move] =
      require(rook == Figure.ROOK1 || rook == Figure.ROOK2)
      allMovesByCoordinatesWeak(0, 1, board, current, turn, Nil, rook) |+|
        allMovesByCoordinatesWeak(0, -1, board, current, turn, Nil, rook) |+|
        allMovesByCoordinatesWeak(1, 0, board, current, turn, Nil, rook) |+|
        allMovesByCoordinatesWeak(-1, 0, board, current, turn, Nil, rook)

    def weakQueenMoves: List[Move] = (weakRookMoves(Figure.ROOK2 /*does not matter*/) |+| weakBishopMoves).map(move => SimpleMove(Figure.QUEEN, move.fromTo))

    def isAvailable(coordinate: Coordinate, color: String): List[Coordinate] = {
      if (coordinate._1 > 8 || coordinate._1 < 1 || coordinate._2 > 8 || coordinate._2 < 1) Nil
      else if (board.board(current._1 - 1)(current._2 - 1).figure.get.color == color) Nil
      else List(coordinate)
    }

    def weakKingMoves: List[Move] = {
      isAvailable((current._1 + 1, current._2 + 1), turn) |+|
        isAvailable((current._1 + 1, current._2), turn) |+|
        isAvailable((current._1 + 1, current._2 - 1), turn) |+|
        isAvailable((current._1, current._2 - 1), turn) |+|
        isAvailable((current._1 - 1, current._2 - 1), turn) |+|
        isAvailable((current._1 - 1, current._2), turn) |+|
        isAvailable((current._1 - 1, current._2 + 1), turn) |+|
        isAvailable((current._1, current._2 + 1), turn)
    }.map(coordinate => SimpleMove(Figure.KING, (current, coordinate)))

    def weakKnightMoves: List[Move] = {
      isAvailable((current._1 + 1, current._2 + 2), turn) |+|
        isAvailable((current._1 + 2, current._2 + 1), turn) |+|
        isAvailable((current._1 + 1, current._2 - 2), turn) |+|
        isAvailable((current._1 - 2, current._2 + 1), turn) |+|
        isAvailable((current._1 - 1, current._2 - 2), turn) |+|
        isAvailable((current._1 - 2, current._2 - 1), turn) |+|
        isAvailable((current._1 + 2, current._2 - 1), turn) |+|
        isAvailable((current._1 - 1, current._2 + 2), turn)
    }.map(coordinate => SimpleMove(Figure.KNIGHT, (current, coordinate)))


    def weakPawnMoves: List[Move] = turn match {
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
            |+| {
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
          } |+| {
            if (board.board(current._1 - 2)(current._2).figure.isEmpty) Nil
            else if (board.board(current._1 - 2)(current._2).figure.get.color == turn) Nil
            else List((current._1 - 1, current._2 + 1))
          }
        }

        (enPassant |+| simpleMove |+| take).map(coordinate => SimpleMove(Figure.PAWN, (current, coordinate)))

      case "black" =>
        def enPassant: List[Coordinate] = previousMove match {
          case Some(figure, ((x1, y1), (x2, y2))) if figure == Figure.PAWN && (x1 == current._1 + 1 && y1 == current._2 - 2) && (y2 == current._2) => List((current._1 + 1, 3))
          case Some(figure, ((x1, y1), (x2, y2))) if figure == Figure.PAWN && (x1 == current._1 - 1 && y1 == current._2 - 2) && (y2 == current._2) => List((current._1 - 1, 3))
          case _ => Nil
        }

        def simpleMove: List[Coordinate] = {
          if (current._2 == 7) {
            if (board.board(current._1 - 1)(current._2 - 2).figure.isEmpty) List((current._1, current._2 - 1))
            else Nil
          } |+| {
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
          } |+| {
            if (board.board(current._1 - 2)(current._2 - 2).figure.isEmpty) Nil
            else if (board.board(current._1 - 2)(current._2 - 2).figure.get.color == turn) Nil
            else List((current._1 - 1, current._2 - 1))
          }
        }

        (enPassant |+| simpleMove |+| take).map(coordinate => SimpleMove(Figure.PAWN, (current, coordinate)))
    }

    board.board(current._1 - 1)(current._2 - 1).figure.get.figure match {
      case Figure.ROOK1 => weakRookMoves(Figure.ROOK1)
      case Figure.ROOK2 => weakRookMoves(Figure.ROOK2)
      case Figure.KNIGHT => weakKnightMoves
      case Figure.BISHOP => weakBishopMoves
      case Figure.QUEEN => weakQueenMoves
      case Figure.KING => weakKingMoves
      case Figure.PAWN => weakPawnMoves
      case Figure.NoCastleROOK => weakRookMoves(Figure.NoCastleROOK)
    }
  }

  def makeMovePrediction(gameInfo: GameInfo, move: Move): GameInfo = {
    val board = gameInfo.board.board
    val updatedBoard = move match {
      case SimpleMove(figure, fromTo) =>
        if (fromTo._1._1 == fromTo._2._1) {
        val columnToUpdate = board(fromTo._1._1 - 1)
        val updatedColumn = columnToUpdate.updated(fromTo._1._2 - 1, Field(fromTo._1, None)).updated(fromTo._2._2 - 1, Field(fromTo._2, board(fromTo._1._1 - 1)(fromTo._1._2 - 1).figure))
        Board(board.updated(fromTo._1._1 - 1, updatedColumn))
      }
        else {
          val columnToUpdate1 = board(fromTo._1._1 - 1)
          val columnToUpdate2 = board(fromTo._2._1 - 1)
          val updated1 = columnToUpdate1.updated(fromTo._1._2 - 1, Field(fromTo._1, None))
          val updated2 = columnToUpdate2.updated(fromTo._2._2 - 1, Field(fromTo._2, board(fromTo._1._1 - 1)(fromTo._1._2 - 1).figure))
          Board(board.updated(fromTo._1._1 - 1, updated1).updated(fromTo._2._1 - 1, updated2))
      }
      case EnPassant(fromTo) => gameInfo.turn match {
        case "white" =>
          val columnToUpdate1 = board(fromTo._1._1 - 1)
          val columnToUpdate2 = board(fromTo._2._1 - 1)
          val updated1 = columnToUpdate1.updated(fromTo._1._2 - 1, Field(fromTo._1, None))
          val updated2 = columnToUpdate2.updated(fromTo._2._2 - 2, Field(fromTo._1, None)).updated(fromTo._2._2 - 1, Field(fromTo._2, Some(FigureWithColor(move.figure, "white"))))
          Board(board.updated(fromTo._1._1 - 1, updated1).updated(fromTo._2._1 - 1, updated2))

        case "black" =>
          val columnToUpdate1 = board(fromTo._1._1 - 1)
          val columnToUpdate2 = board(fromTo._2._1 - 1)
          val updated1 = columnToUpdate1.updated(fromTo._1._2 - 1, Field(fromTo._1, None))
          val updated2 = columnToUpdate2.updated(fromTo._2._2, Field(fromTo._1, None)).updated(fromTo._2._2 - 1, Field(fromTo._2, Some(FigureWithColor(move.figure, "black"))))
          Board(board.updated(fromTo._1._1 - 1, updated1).updated(fromTo._2._1 - 1, updated2))
      }
      case Castle(right, fromTo) => gameInfo.turn match {
        case "white" =>
          if (right: Boolean)
            val updated1 = board(4).updated(0, Field((5, 1), None))
            val updated2 = board(5).updated(0, Field((6, 1), Some(FigureWithColor(Figure.ROOK2, "white"))))
            val updated3 = board(6).updated(0, Field((7, 1), Some(FigureWithColor(Figure.KING, "white"))))
            val updated4 = board(7).updated(0, Field((8, 1), None))
            Board(board.updated(4, updated1).updated(5, updated2).updated(6, updated3).updated(7, updated4))
          else
            val updated1 = board(0).updated(0, Field((1, 1), None))
            val updated2 = board(2).updated(0, Field((3, 1), Some(FigureWithColor(Figure.KING, "white"))))
            val updated3 = board(3).updated(0, Field((4, 1), Some(FigureWithColor(Figure.ROOK1, "white"))))
            val updated4 = board(4).updated(0, Field((5, 1), None))
            Board(board.updated(0, updated1).updated(2, updated2).updated(3, updated3).updated(4, updated4))
        case "black" =>
          if (right: Boolean)
            val updated1 = board(4).updated(7, Field((5, 8), None))
            val updated2 = board(5).updated(7, Field((6, 8), Some(FigureWithColor(Figure.ROOK2, "black"))))
            val updated3 = board(6).updated(7, Field((7, 8), Some(FigureWithColor(Figure.KING, "black"))))
            val updated4 = board(7).updated(7, Field((8, 8), None))
            Board(board.updated(4, updated1).updated(5, updated2).updated(6, updated3).updated(7, updated4))
          else
            val updated1 = board(0).updated(7, Field((1, 8), None))
            val updated2 = board(2).updated(7, Field((3, 8), Some(FigureWithColor(Figure.KING, "black"))))
            val updated3 = board(3).updated(7, Field((4, 8), Some(FigureWithColor(Figure.ROOK1, "black"))))
            val updated4 = board(4).updated(7, Field((5, 8), None))
            Board(board.updated(0, updated1).updated(2, updated2).updated(3, updated3).updated(4, updated4))
      }

    }
    val fieldsWithFigures = gameInfo.fieldsWithFigures
    val updatedFieldsWithFigures: (List[Coordinate], List[Coordinate]) = move match {
      case EnPassant(fromTo) => gameInfo.turn match {
        case "white" => (fromTo._2 :: fieldsWithFigures._1.filter(_ != fromTo._1), fieldsWithFigures._2.filter(_ != (fromTo._2._1, fromTo._2._2 - 1)))
        case "black" => (fieldsWithFigures._1.filter(_ != (fromTo._2._1, fromTo._2._2 + 1)), fromTo._2 :: fieldsWithFigures._2.filter(_ != fromTo._1))
      }
      case Castle(right, fromTo) => gameInfo.turn match {
        case "white" => if (right: Boolean) ((6, 1) :: (7, 1) :: fieldsWithFigures._1.filter(_ != (5, 1)).filter(_ != (8, 1)), fieldsWithFigures._2)
        else ((3, 1) :: (4, 1) :: fieldsWithFigures._1.filter(_ != (1, 1)).filter(_ != (5, 1)), fieldsWithFigures._2)
        case "black" => if (right: Boolean) (fieldsWithFigures._1, (6, 8) :: (7, 8) :: fieldsWithFigures._1.filter(_ != (5, 8)).filter(_ != (8, 8)))
        else (fieldsWithFigures._1, (3, 8) :: (4, 8) :: fieldsWithFigures._1.filter(_ != (1, 8)).filter(_ != (5, 8)))
      }
      case SimpleMove(figure, fromTo) => gameInfo.turn match {
        case "white" => (fromTo._2 :: fieldsWithFigures._1.filter(_ != fromTo._1), fieldsWithFigures._2.filter(_ != fromTo._2))
        case "black" => (fieldsWithFigures._1.filter(_ != fromTo._2), fromTo._2 :: fieldsWithFigures._2.filter(_ != fromTo._1))
      }
      case Promotion(fromTo, figure) => gameInfo.turn match {
        case "white" => (fromTo._2 :: fieldsWithFigures._1.filter(_ != fromTo._1), fieldsWithFigures._2.filter(_ != fromTo._2))
        case "black" => (fieldsWithFigures._1.filter(_ != fromTo._2), fromTo._2 :: fieldsWithFigures._2.filter(_ != fromTo._1))
      }
    }
    val updatedTurn = if (gameInfo.turn == "white") "black" else "white"
    val kingCoordinates = gameInfo.kingCoordinates
    val updatedKingCoordinates = gameInfo.turn match {
      case "white" => if (move.figure == Figure.KING) (move.fromTo._2, kingCoordinates._2) else kingCoordinates
      case "black" => if (move.figure == Figure.KING) (kingCoordinates._1, move.fromTo._2) else kingCoordinates
    }

    val isCastleAvailable = gameInfo.isCastleAvailable
    val updatedCastle = gameInfo.turn match {
      case "white" =>
        if (move.figure == Figure.KING)
          isCastleAvailable.updated(0, false).updated(1, false)
        else if (move.figure == Figure.ROOK1)
          isCastleAvailable.updated(0, false)
        else if (move.figure == Figure.ROOK2)
          isCastleAvailable.updated(1, false)
        else isCastleAvailable
      case "black" =>
        if (move.figure == Figure.KING)
          isCastleAvailable.updated(2, false).updated(3, false)
        else if (move.figure == Figure.ROOK1)
          isCastleAvailable.updated(2, false)
        else if (move.figure == Figure.ROOK2)
          isCastleAvailable.updated(3, false)
        else isCastleAvailable
    }
    val updatedPreviousMove = Some(move.figure, move.fromTo)
    GameInfo(updatedBoard, gameInfo.moves + 1, updatedPreviousMove, updatedCastle, updatedKingCoordinates, updatedFieldsWithFigures, updatedTurn)
  }

  def isPositionLegal(gameInfo: GameInfo): Boolean = gameInfo.turn match {
    case "white" => !gameInfo.fieldsWithFigures._1.flatMap(a => availableMovesFromWeak(gameInfo, a)).contains(gameInfo.kingCoordinates._2)
    case "black" => !gameInfo.fieldsWithFigures._2.flatMap(a => availableMovesFromWeak(gameInfo, a)).contains(gameInfo.kingCoordinates._1)
  }
  def getGame: F[GameInfo] = game.get

  def isEnded(gameInfo: GameInfo): F[GameStatus] = gameInfo.turn.match {
    case "white" => gameInfo.fieldsWithFigures._1.flatTraverse(availableMovesFrom(gameInfo, _)).map {
      case Nil if !isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, (gameInfo.kingCoordinates._1, gameInfo.kingCoordinates._1)))) => "black win"
      case Nil => "draw"
      case _ => "game continuing"
    }
    case "black" => gameInfo.fieldsWithFigures._2.flatTraverse(availableMovesFrom(gameInfo, _)).map {
      case Nil if !isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, (gameInfo.kingCoordinates._2, gameInfo.kingCoordinates._2)))) => "white win"
      case Nil => "draw"
      case _ => "game continuing"
    }
  }


  def makeMove(move: Move): F[GameStatus] = for {
    gameInfoOld <- game.get
    gameInfoNew <- makeMovePrediction(gameInfoOld, move).pure[F]
    isEnded <- isEnded(gameInfoNew)
    _ <- game.set(gameInfoNew)
  } yield isEnded
}


object GameWorker {
  def apply[F[_] : Concurrent](players: (UUID, UUID), id: UUID): F[GameWorker[F]] = for {
    game <- Ref.of[F, GameInfo](GameInfo(defaultBoard))
  } yield new GameWorker[F](game)(id)
}


