package fr.kalman.bliebchessrecorder.logic

// TODO: implement apply to act as a map
// TODO: implement adding/removing/moving piece (square coords as key)
// TODO: Forbid duplicate keys (2 pieces on same square)
// TODO: implement apply to act as a map
abstract class Board {
  def head: PieceOnSquare
  def tail: Board
  def isEmpty: Boolean

  def pieceCount : Int = if (isEmpty) 0 else 1 + tail.pieceCount

  def :: (piece: PieceOnSquare) = new ::(piece, this)
}

case object End extends Board {
  def isEmpty = true
  def head = throw new NoSuchElementException
  def tail = throw new NoSuchElementException
}

final case class ::(head: PieceOnSquare, tail: Board) extends Board {
  require(tail.pieceCount < 64, "No place left on the board")
  val isEmpty = false
}

