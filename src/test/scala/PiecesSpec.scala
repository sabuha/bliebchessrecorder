import fr.kalman.bliebchessrecorder.logic._
import fr.kalman.bliebchessrecorder.logic.Color._
import fr.kalman.bliebchessrecorder.logic.Piece

class PiecesSpec extends org.scalatest.FreeSpec {

  "2 Pieces are equals if they are same kind and color" in {
    val allPieces = List(
      Pawn(WHITE), Rook(WHITE), Knight(WHITE), Bishop(WHITE), Queen(WHITE), King(WHITE),
      Pawn(BLACK), Rook(BLACK), Knight(BLACK), Bishop(BLACK), Queen(BLACK), King(BLACK))

    var i = 0
    allPieces.map(p1 => allPieces.map(p2 => {
      i = i + 1
      assert(
        (p1, p2) match {
          // Pieces of different color cannot be equal
          case (p1: Piece, p2: Piece) if p1.color != p2.color => p1 != p2
          // Remaining matches for pieces of same color
          case (Pawn(_), Pawn(_)) => p1 == p2
          case (Rook(_), Rook(_)) => p1 == p2
          case (Knight(_), Knight(_)) => p1 == p2
          case (Bishop(_), Bishop(_)) => p1 == p2
          case (Queen(_), Queen(_)) => p1 == p2
          case (King(_), King(_)) => p1 == p2
          case _ => p1 != p2
        })
    }))
  }

  "Each piece is identified by a letter" - {
    assert(Rook(WHITE).toString === "R")
    assert(Knight(BLACK).toString === "n")
    assert(Bishop(WHITE).toString === "B")
    assert(Queen(BLACK).toString === "q")
    assert(King(WHITE).toString === "K")
  }

  "Piece hashcode" - {
    "goes from 1 for the white bishop" in {
      assert(Bishop(WHITE).hashCode === 1)
    }
    "to 6 for the white rook" in {
      assert(Rook(WHITE).hashCode === 6)
    }
    "is doubled for a black piece" in {
      assert(King(BLACK).hashCode === 2*King(WHITE).hashCode)
    }
    "so that the maximum possible is" in {
      assert(Rook(BLACK).hashCode === 12)
    }
  }
}
