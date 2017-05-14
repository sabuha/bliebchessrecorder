package fr.kalman.bliebchessrecorder.logic

case class Move(fromSq: SquareCoord, toSq: SquareCoord) {
  require(fromSq != toSq, "identical starting and destination squares")
}

