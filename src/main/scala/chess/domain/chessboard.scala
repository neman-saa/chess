package chess.domain

import chessboard.Figure._

object chessboard {

  enum Figure {
    case PAWN, BISHOP, KNIGHT, ROOK1, ROOK2,  KING, QUEEN
  }

  case class FigureWithColor(figure: Figure, color: String)

  case class Field(coordinates: (Int, Int), figure: Option[FigureWithColor])

  case class Board(board: List[List[Field]])

  val defaultBoard: Board = Board(
    List(
      List(
        Field((1, 1), Some(FigureWithColor(ROOK1, "White"))), 
        Field((2, 1), Some(FigureWithColor(KNIGHT, "White"))),
        Field((3, 1), Some(FigureWithColor(BISHOP, "White"))),
        Field((4, 1), Some(FigureWithColor(QUEEN, "White"))), 
        Field((5, 1), Some(FigureWithColor(KING, "White"))),
        Field((6, 1), Some(FigureWithColor(BISHOP, "White"))),
        Field((7, 1), Some(FigureWithColor(KNIGHT, "White"))), 
        Field((8, 1), Some(FigureWithColor(ROOK2, "White")))),
      (1 to 8).toList.map(x => Field((x, 2), Some(FigureWithColor(PAWN, "White")))),
      (1 to 8).toList.map(x => Field((x, 3), None)),
      (1 to 8).toList.map(x => Field((x, 4), None)),
      (1 to 8).toList.map(x => Field((x, 5), None)),
      (1 to 8).toList.map(x => Field((x, 6), None)),
      (1 to 8).toList.map(x => Field((x, 7), Some(FigureWithColor(PAWN, "Black")))),
      List(
        Field((1, 1), Some(FigureWithColor(ROOK1, "Black"))), 
        Field((2, 1), Some(FigureWithColor(KNIGHT, "Black"))),
        Field((3, 1), Some(FigureWithColor(BISHOP, "Black"))), 
        Field((4, 1), Some(FigureWithColor(QUEEN, "Black"))),
        Field((5, 1), Some(FigureWithColor(KING, "Black"))), 
        Field((6, 1), Some(FigureWithColor(BISHOP, "Black"))),
        Field((7, 1), Some(FigureWithColor(KNIGHT, "Black"))), 
        Field((8, 1), Some(FigureWithColor(ROOK2, "Black")))),
    )
  )
}
