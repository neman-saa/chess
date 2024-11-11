package chess.domain

import chessboard.Figure._

object chessboard {

  enum Figure {
    case PAWN, BISHOP, KNIGHT, ROOK1, ROOK2, KING, QUEEN, NoCastleROOK
  }

  type Coordinate = (Int, Int)
  case class FigureWithColor(figure: Figure, color: String, coordinate: Coordinate)
  case class Board(board: List[FigureWithColor])

  val defaultBoard: Board = Board(
    List(
      FigureWithColor(Figure.ROOK1, "white", (1, 1)),
      FigureWithColor(Figure.KNIGHT, "white", (2, 1)),
      FigureWithColor(Figure.BISHOP, "white", (3, 1)),
      FigureWithColor(Figure.QUEEN, "white", (4, 1)),
      FigureWithColor(Figure.KING, "white", (5, 1)),
      FigureWithColor(Figure.BISHOP, "white", (6, 1)),
      FigureWithColor(Figure.KNIGHT, "white", (7, 1)),
      FigureWithColor(Figure.ROOK2, "white", (8, 1)),
      FigureWithColor(Figure.ROOK1, "black", (1, 8)),
      FigureWithColor(Figure.KNIGHT, "black", (2, 8)),
      FigureWithColor(Figure.BISHOP, "black", (3, 8)),
      FigureWithColor(Figure.QUEEN, "black", (4, 8)),
      FigureWithColor(Figure.KING, "black", (5, 8)),
      FigureWithColor(Figure.BISHOP, "black", (6, 8)),
      FigureWithColor(Figure.KNIGHT, "black", (7, 8)),
      FigureWithColor(Figure.ROOK2, "black", (8, 8)),
    ) ::: (1 to 8).toList.map(x => FigureWithColor(Figure.PAWN, "white", (x, 2)))
      ::: (1 to 8).toList.map(x => FigureWithColor(Figure.PAWN, "black", (x, 7)))
  )
}
