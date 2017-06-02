import fr.kalman.bliebchessrecorder.logic.Color._
import fr.kalman.bliebchessrecorder.logic._


class BoardSpec extends org.scalatest.FreeSpec {
  val king_on_e1: PieceOnSquare = PieceOnSquare("e1", King(WHITE))
  val king_on_e8: PieceOnSquare = PieceOnSquare("e8", King(BLACK))
  val board: Board = king_on_e1 :: king_on_e8 :: End

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
        board = PieceOnSquare(SquareCoord(i, j), King(WHITE)) :: board
      }

      assert(board.pieceCount === 64)
    }
    "but no more than that." in {
      intercept[IllegalArgumentException] {
        PieceOnSquare(SquareCoord(7, 7), King(WHITE)) :: board
      }
    }
  }

  "A piece" - {
    "can be found on the board based on its coordinates" in {
      assert(board("e1").isDefined)
      assert(board("e2").isEmpty)
      assert(board("e8").get === King(BLACK))
    }
    "cannot be placed on an occupied square" in {
      intercept[IllegalArgumentException] {
        PieceOnSquare("e1", Pawn(BLACK)) :: king_on_e1 :: End
      }
    }
    "can be removed based on its coordinates" in {
      assert(board.removePiece("e1")("e1").isEmpty)
    }
    "cannot be removed from a square it does not occupy" in {
      intercept[IllegalArgumentException] {
        val board = king_on_e1 :: king_on_e8 :: End
        board.removePiece("e2")
      }
    }
    "can be moved to an empty square" in {
      val newBoard = board.movePiece("e1", "e2")
      assert(newBoard("e2")===board("e1"))
      assert(newBoard("e1").isEmpty)
    }
    "cannot be moved to an occupie square, because captures are not natively supported by Board" in {
      intercept[IllegalArgumentException] {
        board.movePiece("e1", "e8")
      }
    }
  }
}
