package fr.kalman.bliebchessrecorder.logic

import fr.kalman.bliebchessrecorder.logic.Board.PieceOnSquare

object Board {
  type PieceOnSquare = (SquareCoord, Piece)
}

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

