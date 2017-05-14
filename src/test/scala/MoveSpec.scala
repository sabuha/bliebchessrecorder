import fr.kalman.bliebchessrecorder.logic.{Move, SquareCoord}

class MoveSpec extends org.scalatest.FreeSpec {
  "A Move" - {
    "produce IllegalArgumentException if source and destination squares are same" in {
      intercept[IllegalArgumentException] {
        Move("e2", "e2")
      }
    }
  }
}
