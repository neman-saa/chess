package coreTests

import cats.effect.testing.scalatest.AsyncIOSpec
import chess.core.GameWorker
import org.scalatest.freespec.{AsyncFreeSpec, AsyncFreeSpecLike}
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import cats.effect.kernel.Resource
import chess.domain.chessboard.{Board, Figure, FigureWithColor}
import chess.domain.game.GameStatus.{BlackWin, Draw, WhiteWin}
import chess.domain.game.Move.SimpleMove

import java.util.UUID
class GameWorkerSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers{

  "gameWorker" - {

    val drawBoard = Board(List(
      FigureWithColor(Figure.KING, "white", (1, 1)),
      FigureWithColor(Figure.QUEEN, "black", (3, 2)),
      FigureWithColor(Figure.KING, "black", (8, 8))))

    "game worker should give SimpleMove(e4) when using availableMovesFrom on defaultBoard" in {
      Resource.eval(GameWorker[IO](UUID.randomUUID())).use { worker =>
        val movesF = worker.availableMovesFrom((5, 2))
        movesF.asserting(_ shouldBe List(SimpleMove(Figure.PAWN,((5,2),(5,3))), SimpleMove(Figure.PAWN,((5,2),(5,4)))))
      }
    }

    "game worker should give game status 'black win' when black make mate" in {
      Resource.eval(GameWorker[IO](UUID.randomUUID())).use { worker =>
       for {
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((6, 2), (6, 3))))
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((5, 7), (5, 6))))
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((7, 2), (7, 4))))
          status <- worker.makeMove(SimpleMove(Figure.QUEEN, ((4, 8), (8, 4))))
        } yield status shouldBe BlackWin
      }
    }

    "game worker should give game status 'white win' when white make mate" in {
      Resource.eval(GameWorker[IO](UUID.randomUUID())).use { worker =>
        for {
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((5, 2), (5, 3))))
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((6, 7), (6, 6))))
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((3, 2), (3, 3))))
          _ <- worker.makeMove(SimpleMove(Figure.PAWN, ((7, 7), (7, 5))))
          status <- worker.makeMove(SimpleMove(Figure.QUEEN, ((4, 1), (8, 5))))
        } yield status shouldBe WhiteWin
      }
    }

    "game worker should give game status 'draw' when stalemate" in {
      Resource.eval(GameWorker[IO](UUID.randomUUID(), drawBoard)).use { worker =>
        for {
          game <- worker.game.updateAndGet(gameInfo => gameInfo.copy(kingCoordinates = ((1, 1), (8, 8))).copy(isCastleAvailable = List(false, false, false, false)))
          status <- worker.isEnded(game)
        } yield status shouldBe Draw
      }
    }
  }
}
