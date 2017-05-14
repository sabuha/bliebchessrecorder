import fr.kalman.bliebchessrecorder.logic.Color._
import fr.kalman.bliebchessrecorder.logic._


class BoardSpec extends org.scalatest.FreeSpec {
  val king_on_e1: PieceOnSquare = PieceOnSquare("e1", King(WHITE))
  val king_on_e8: PieceOnSquare = PieceOnSquare("e8", King(BLACK))

  "A board" - {
    "counts zero piece when empty" in {
      assert(End.pieceCount === 0)
    }
    "counts 1 piece when placing a king on an empty board" in {
      assert(::(king_on_e1, End).pieceCount === 1)
    }
    "has a length of 2 when placing 2 kings on an empty board" in {
      assert((king_on_e1 :: king_on_e8 :: End).pieceCount === 2)
    }
    var board: Board = fr.kalman.bliebchessrecorder.logic.End
    "can hold a maximum of 64 pieces" in {
      for (i <- 0 until 8; j <- 0 until 8) {
        board = PieceOnSquare(SquareCoord(i,j), King(WHITE)) :: board
      }

      assert(board.pieceCount === 64)
    }
    "but no more than that" in {
      intercept[IllegalArgumentException] {
        PieceOnSquare(SquareCoord(7, 7), King(WHITE)) :: board
      }
    }
  }
}
