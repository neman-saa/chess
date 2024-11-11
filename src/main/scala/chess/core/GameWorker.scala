package chess.core

import java.util.UUID

import scala.annotation.tailrec

import cats.effect.kernel.Concurrent
import cats.effect.syntax.*
import cats.effect.Ref
import cats.instances.list.*
import cats.kernel.Previous
import cats.syntax.all.*
import chess.domain.chessboard._
import chess.domain.game.*
import chess.domain.game.GameStatus.*
import chess.domain.game.Move.*

class GameWorker[F[_]: Concurrent](val game: Ref[F, GameInfo])(val id: UUID) {
  type Coordinate = (Int, Int)

  def getGameId: F[UUID] = id.pure[F]

  private def availableMovesFromPrivate(gameInfo: GameInfo, from: Coordinate): F[List[Move]] = {

    val isCastleAvailable = gameInfo.isCastleAvailable

    val board = gameInfo.board.board

    def castle(turn: String, right: Boolean): List[Move] = (turn, right) match {
      case ("white", true) if gameInfo.isCastleAvailable(1) =>
        if (
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (7, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (5, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (6, 1))))) &&
          !board.exists(figure => figure.coordinate == (6, 1) || figure.coordinate == (7, 1))
        ) List(Castle(true, ((5, 1), (7, 1))))
        else Nil
      case ("black", true) if gameInfo.isCastleAvailable(3) =>
        if (
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (7, 8))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (5, 8))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (6, 8))))) &&
          !board.exists(figure => figure.coordinate == (6, 8) || figure.coordinate == (7, 8))
        ) List(Castle(true, ((5, 8), (7, 8))))
        else Nil
      case ("black", false) if gameInfo.isCastleAvailable(2) =>
        if (
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (5, 8))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (4, 8))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 8), (3, 8))))) &&
          !board
            .exists(figure => figure.coordinate == (2, 8) || figure.coordinate == (3, 8) || figure.coordinate == (4, 8))
        ) List(Castle(false, ((5, 8), (3, 8))))
        else Nil
      case ("white", false) if gameInfo.isCastleAvailable(0) =>
        if (
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (5, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (4, 1))))) &&
          isPositionLegal(makeMovePrediction(gameInfo, SimpleMove(Figure.KING, ((5, 1), (3, 1))))) &&
          !board
            .exists(figure => figure.coordinate == (2, 1) || figure.coordinate == (3, 1) || figure.coordinate == (4, 1))
        ) List(Castle(false, ((5, 1), (3, 1))))
        else Nil
      case _ => Nil
    }

    (castle(gameInfo.turn, true) |+| castle(gameInfo.turn, false) |+| availableMovesFromWeak(gameInfo, from))
      .filter(move => isPositionLegal(makeMovePrediction(gameInfo, move)))
      .pure[F]
  }

  private def availableMovesFromWeak(gameInfo: GameInfo, from: Coordinate): List[Move] = {
    val board        = gameInfo.board
    val current      = from
    val turn         = gameInfo.turn
    val previousMove = gameInfo.previousMove

    @tailrec
    def allMovesByCoordinatesWeak(
        x: Int,
        y: Int,
        board: Board,
        localCurrent: Coordinate,
        color: String,
        moves: List[Move] = Nil,
        figure: Figure
    ): List[Move] =
      if (localCurrent._1 + x > 8 || localCurrent._1 + x < 1 || localCurrent._2 + y > 8 || localCurrent._2 + y < 1)
        moves
      else
        board.board.filter(_.coordinate == (localCurrent._1 + x, localCurrent._2 + y)) match {
          case Nil =>
            allMovesByCoordinatesWeak(
              x,
              y,
              board,
              (localCurrent._1 + x, localCurrent._2 + y),
              color,
              SimpleMove(figure, (current, (localCurrent._1 + x, localCurrent._2 + y))) :: moves,
              figure
            )
          case List(FigureWithColor(_, cvet, _)) if cvet == color => moves
          case _                                                  => SimpleMove(figure, (current, (localCurrent._1 + x, localCurrent._2 + y))) :: moves
        }

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

    def weakQueenMoves: List[Move] = (weakRookMoves(Figure.ROOK2 /*does not matter*/ ) |+| weakBishopMoves).map(move =>
      SimpleMove(Figure.QUEEN, move.fromTo)
    )

    def isAvailable(coordinate: Coordinate, color: String): List[Coordinate] =
      if (coordinate._1 > 8 || coordinate._1 < 1 || coordinate._2 > 8 || coordinate._2 < 1) Nil
      else
        board.board.filter(_.coordinate == coordinate) match {
          case List(FigureWithColor(_, cvet, _)) if cvet == color => Nil
          case _                                                  => List(coordinate)
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

    def weakPawnMoves: List[Move] = {

      def enPassant: List[Move] = (turn, previousMove) match {
        case ("white", Some(SimpleMove(Figure.PAWN, ((from1, from2), (_, to2)))))
            if from1 == current._1 + 1 && from2 == current._2 + 2 && to2 == current._2 =>
          List(EnPassant((current, (current._1 + 1, current._2 + 1))))
        case ("white", Some(SimpleMove(Figure.PAWN, ((from1, from2), (_, to2)))))
            if from1 == current._1 - 1 && from2 == current._2 + 2 && to2 == current._2 =>
          List(EnPassant((current, (current._1 - 1, current._2 + 1))))
        case ("black", Some(SimpleMove(Figure.PAWN, ((from1, from2), (_, to2)))))
            if from1 == current._1 + 1 && from2 == current._2 - 2 && to2 == current._2 =>
          List(EnPassant((current, (current._1 + 1, current._2 - 1))))
        case ("black", Some(SimpleMove(Figure.PAWN, ((from1, from2), (_, to2)))))
            if from1 == current._1 - 1 && from2 == current._2 + -2 && to2 == current._2 =>
          List(EnPassant((current, (current._1 - 1, current._2 - 1))))
        case _ => Nil
      }

      def simpleMove: List[Move] = turn match {
        case "white" =>
          (current._2, board.board.filter(_.coordinate == (current._1, current._2 + 1))) match {
            case (2, Nil) if !board.board.exists(_.coordinate == (current._1, current._2 + 2)) =>
              List(
                SimpleMove(Figure.PAWN, (current, (current._1, current._2 + 1))),
                SimpleMove(Figure.PAWN, (current, (current._1, current._2 + 2)))
              )
            case (7, Nil) =>
              List(Figure.KNIGHT, Figure.BISHOP, Figure.QUEEN, Figure.NoCastleROOK).map(figure =>
                Promotion((current, (current._1, current._2 + 1)), figure)
              )
            case (_, Nil) =>
              List(SimpleMove(Figure.PAWN, (current, (current._1, current._2 + 1))))
            case _ => Nil
          }
        case "black" =>
          (current._2, board.board.filter(_.coordinate == (current._1, current._2 - 1))) match {
            case (7, Nil) if !board.board.exists(_.coordinate == (current._1, current._2 - 2)) =>
              List(
                SimpleMove(Figure.PAWN, (current, (current._1, current._2 - 1))),
                SimpleMove(Figure.PAWN, (current, (current._1, current._2 - 2)))
              )
            case (2, Nil) =>
              List(Figure.KNIGHT, Figure.BISHOP, Figure.QUEEN, Figure.NoCastleROOK).map(figure =>
                Promotion((current, (current._1, current._2 - 1)), figure)
              )
            case (_, Nil) =>
              List(SimpleMove(Figure.PAWN, (current, (current._1, current._2 - 1))))
            case _ => Nil
          }
      }

      def take: List[Move] = turn match {
        case "white"
            if board.board
              .exists(figure => figure.coordinate == (current._1 + 1, current._2 + 1) && figure.color == "black") =>
          List(SimpleMove(Figure.PAWN, (current, (current._1 + 1, current._2 + 1))))
        case "white"
            if board.board
              .exists(figure => figure.coordinate == (current._1 - 1, current._2 + 1) && figure.color == "black") =>
          List(SimpleMove(Figure.PAWN, (current, (current._1 - 1, current._2 + 1))))
        case "black"
            if board.board
              .exists(figure => figure.coordinate == (current._1 + 1, current._2 - 1) && figure.color == "white") =>
          List(SimpleMove(Figure.PAWN, (current, (current._1 + 1, current._2 - 1))))
        case "black"
            if board.board
              .exists(figure => figure.coordinate == (current._1 - 1, current._2 - 1) && figure.color == "white") =>
          List(SimpleMove(Figure.PAWN, (current, (current._1 - 1, current._2 - 1))))
        case _ => Nil
      }

      enPassant |+| simpleMove |+| take
    }

    board.board.filter(figure => figure.coordinate == current).map(_.figure) match {
      case Figure.ROOK1 :: Nil        => weakRookMoves(Figure.ROOK1)
      case Figure.ROOK2 :: Nil        => weakRookMoves(Figure.ROOK2)
      case Figure.KNIGHT :: Nil       => weakKnightMoves
      case Figure.BISHOP :: Nil       => weakBishopMoves
      case Figure.QUEEN :: Nil        => weakQueenMoves
      case Figure.KING :: Nil         => weakKingMoves
      case Figure.PAWN :: Nil         => weakPawnMoves
      case Figure.NoCastleROOK :: Nil => weakRookMoves(Figure.NoCastleROOK)
      case _                          => Nil
    }
  }

  private def makeMovePrediction(gameInfo: GameInfo, move: Move): GameInfo = {
    val board = gameInfo.board.board
    val turn  = gameInfo.turn
    val updatedBoard = move match {

      case SimpleMove(figure, fromTo) =>
        Board(
          FigureWithColor(figure, turn, fromTo._2) :: board.filter(figure =>
            figure.coordinate != fromTo._1 && figure.coordinate != fromTo._2
          )
        )

      case EnPassant(fromTo) =>
        gameInfo.turn match {
          case "white" =>
            Board(
              FigureWithColor(Figure.PAWN, "white", fromTo._2) :: board
                .filter(_.coordinate != fromTo._1)
                .filter(_.coordinate != (fromTo._2._1, fromTo._2._2 - 1))
            )
          case "black" =>
            Board(
              FigureWithColor(Figure.PAWN, "black", fromTo._2) :: board
                .filter(_.coordinate != fromTo._1)
                .filter(_.coordinate != (fromTo._2._1, fromTo._2._2 + 1))
            )
        }

      case Castle(right, fromTo) =>
        (turn, right) match {
          case ("white", true) =>
            Board(
              FigureWithColor(Figure.KING, "white", (7, 1)) :: FigureWithColor(Figure.ROOK2, "white", (6, 1)) :: board
                .filter(_.coordinate != (5, 1))
                .filter(_.coordinate != (8, 1))
            )
          case ("black", true) =>
            Board(
              FigureWithColor(Figure.KING, "black", (7, 8)) :: FigureWithColor(Figure.ROOK2, "black", (6, 8)) :: board
                .filter(_.coordinate != (5, 8))
                .filter(_.coordinate != (8, 8))
            )
          case ("white", false) =>
            Board(
              FigureWithColor(Figure.KING, "white", (3, 1)) :: FigureWithColor(Figure.ROOK2, "white", (4, 1)) :: board
                .filter(_.coordinate != (5, 1))
                .filter(_.coordinate != (1, 1))
            )
          case ("black", false) =>
            Board(
              FigureWithColor(Figure.KING, "black", (3, 8)) :: FigureWithColor(Figure.ROOK2, "black", (4, 8)) :: board
                .filter(_.coordinate != (5, 8))
                .filter(_.coordinate != (1, 8))
            )
          case (_, _) => Board(Nil)
        }

      case Promotion(fromToo, promotesTo) =>
        Board(FigureWithColor(promotesTo, turn, fromToo._2) :: board.filter(_.coordinate != fromToo._1))
    }

    val updatedTurn = if (gameInfo.turn == "white") "black" else "white"

    val kingCoordinates = gameInfo.kingCoordinates

    val updatedKingCoordinates = gameInfo.turn match {
      case "white" => if (move.figure == Figure.KING) (move.fromTo._2, kingCoordinates._2) else kingCoordinates
      case "black" => if (move.figure == Figure.KING) (kingCoordinates._1, move.fromTo._2) else kingCoordinates
    }

    val isCastleAvailable = gameInfo.isCastleAvailable
    val updatedCastle = (gameInfo.turn, move.figure) match {
      case ("white", Figure.KING) =>
        isCastleAvailable.updated(0, false).updated(1, false)
      case ("white", Figure.ROOK1) =>
        isCastleAvailable.updated(0, false)
      case ("white", Figure.ROOK2) =>
        isCastleAvailable.updated(1, false)
      case ("black", Figure.KING) =>
        isCastleAvailable.updated(2, false).updated(3, false)
      case ("black", Figure.ROOK1) =>
        isCastleAvailable.updated(2, false)
      case ("black", Figure.ROOK2) =>
        isCastleAvailable.updated(3, false)
      case (_, _) => isCastleAvailable
    }
    val updatedPreviousMove = Some(move)

    val fiftyMovesRule = gameInfo.fiftyMovesRole
    val updatedFMR = move.match {
      case move if move.figure == Figure.PAWN                                  => 0
      case move if gameInfo.board.board.exists(_.coordinate == move.fromTo._2) => 0
      case _                                                                   => fiftyMovesRule + 1
    }
    GameInfo(
      updatedBoard,
      gameInfo.moves + 1,
      updatedPreviousMove,
      updatedCastle,
      updatedKingCoordinates,
      updatedTurn,
      updatedFMR
    )
  }

  private def isPositionLegal(gameInfo: GameInfo): Boolean = gameInfo.turn match {
    case "white" =>
      !gameInfo.board.board
        .filter(_.color == "white")
        .flatMap(figure => availableMovesFromWeak(gameInfo, figure.coordinate))
        .exists(move => move.fromTo._2 == gameInfo.kingCoordinates._2)
    case "black" =>
      !gameInfo.board.board
        .filter(_.color == "black")
        .flatMap(figure => availableMovesFromWeak(gameInfo, figure.coordinate))
        .exists(move => move.fromTo._2 == gameInfo.kingCoordinates._1)
  }
  def getGame: F[GameInfo] = game.get

  def isEnded(gameInfo: GameInfo): F[GameStatus] = {
    val board                        = gameInfo.board.board
    val (whiteFigures, blackFigures) = board.partition(_.color == "white")

    (whiteFigures.length, blackFigures.length) match {
      case (1, 1) => Draw.pure[F]
      case (2, 1) if whiteFigures.exists(piece => piece.figure == Figure.KNIGHT || piece.figure == Figure.BISHOP) =>
        Draw.pure[F]
      case (1, 2) if blackFigures.exists(piece => piece.figure == Figure.KNIGHT || piece.figure == Figure.BISHOP) =>
        Draw.pure[F]
      case (2, 2)
          if whiteFigures.exists(piece => piece.figure == Figure.KNIGHT || piece.figure == Figure.BISHOP) &&
            blackFigures.exists(piece => piece.figure == Figure.KNIGHT || piece.figure == Figure.BISHOP) =>
        Draw.pure[F]
      case _ if gameInfo.fiftyMovesRole <= 100 =>
        gameInfo.turn.match {
          case "white" =>
            gameInfo.board.board
              .filter(_.color == "white")
              .flatTraverse(figure => availableMovesFromPrivate(gameInfo, figure.coordinate))
              .map {
                case Nil if !isPositionLegal(gameInfo.copy(turn = "black")) => BlackWin
                case Nil                                                    => Draw
                case _                                                      => GameContinuing
              }
          case "black" =>
            gameInfo.board.board
              .filter(_.color == "black")
              .flatTraverse(figure => availableMovesFromPrivate(gameInfo, figure.coordinate))
              .map {
                case Nil if !isPositionLegal(gameInfo.copy(turn = "white")) => WhiteWin
                case Nil                                                    => Draw
                case _                                                      => GameContinuing
              }
        }
      case _ => Draw.pure[F]
    }
  }

  def makeMove(move: Move): F[GameStatus] = for {
    gameInfoOld <- game.get
    isAvailable <- availableMovesFrom(move.fromTo._1).map(_.contains(move))
    status <-
      if isAvailable then {
        val gameInfoNew = makeMovePrediction(gameInfoOld, move)
        for {
          _      <- game.set(gameInfoNew)
          status <- isEnded(gameInfoNew)
        } yield status
      } else NotAValidMove.pure[F]
  } yield status

  def availableMovesFrom(coordinate: Coordinate): F[List[Move]] = for {
    gameInfo <- game.get
    moves    <- availableMovesFromPrivate(gameInfo, coordinate)
  } yield moves
}

object GameWorker {
  def apply[F[_]: Concurrent](id: UUID, board: Board = defaultBoard): F[GameWorker[F]] = for {
    game <- Ref.of[F, GameInfo](GameInfo(board))
  } yield new GameWorker[F](game)(id)
}
