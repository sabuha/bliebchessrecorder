import fr.kalman.bliebchessrecorder.logic.SquareCoord
import fr.kalman.bliebchessrecorder.logic.SquareCoord.stringToSquareCoord

class SquareCoordSpec extends org.scalatest.FreeSpec {
  "Square coordinates" - {

    "when outside the board" - {
      "should produce IllegalArgumentException" in {
        intercept[IllegalArgumentException] {
          SquareCoord(8, 8)
        }
      }
    }

    "when on the board" - {
      "should produce a SquareCoord instance" in {
        SquareCoord(0,0)
        SquareCoord(7,7)
      }
    }
  }

  "A square index" - {
    "should be equal to 0 for a1" in {
      assert("a1".index === 0)
    }
    "should be equal to 63 for h8" in {
      assert("h8".index === 63)
    }
    "should produce IllegalArgumentException for h9" - {
      intercept[IllegalArgumentException] {
        val sq:SquareCoord = "h9"
      }
    }
    "should produce IllegalArgumentException for empty string coordinates" - {
      intercept[IllegalArgumentException] {
        val sq:SquareCoord = ""
      }
    }
    "should produce IllegalArgumentException for invalid coordinates as Nf3" - {
      intercept[IllegalArgumentException] {
        val sq:SquareCoord = "Nf3"
      }
    }
    "should produce IllegalArgumentException for coordinates with capital letter" - {
      intercept[IllegalArgumentException] {
        val sq:SquareCoord = "F3"
      }
    }
  }

  "A square" - {
    "on the south-west of d5 is c4" in {
      assert("d5".relativeSquare(-1,-1) === Some(stringToSquareCoord("c4")))
    }
    "initialized as f3 should print itself as f3" in {
      assert(stringToSquareCoord("f3").toString == "f3")
    }
  }
}
