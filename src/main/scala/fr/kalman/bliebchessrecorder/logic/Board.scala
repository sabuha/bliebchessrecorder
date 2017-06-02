package fr.kalman.bliebchessrecorder.logic

abstract class Board {
  def head: PieceOnSquare

  def tail: Board

  def isEmpty: Boolean

  def pieceCount: Int = if (isEmpty) 0 else 1 + tail.pieceCount

  def ::(piece: PieceOnSquare) = new ::(piece, this)

  def apply(coord: SquareCoord): Option[Piece] = {
    def go(board: Board): Option[Piece] = board match {
      case End => None
      case x :: _ if x.coord == coord => Some(x.piece)
      case _ :: xs => go(xs)
    }

    go(this)
  }

  def filter(f: PieceOnSquare => Boolean): Board = this match {
    case End => End
    case x :: _ if f(x) => head :: (tail filter f)
    case _ => tail filter f
  }

  def removePiece(coord: SquareCoord): Board = {
    require(apply(coord).isDefined, "No piece to remove at these coordinates")
    filter(_.coord != coord)
  }

  def movePiece(fromSq: SquareCoord, toSq: SquareCoord): Board =
    PieceOnSquare(toSq, this(fromSq).get) :: removePiece(fromSq)
}

case object End extends Board {
  def isEmpty = true
  def head = throw new NoSuchElementException
  def tail = throw new NoSuchElementException
}

final case class ::(head: PieceOnSquare, tail: Board) extends Board {
  require(tail.pieceCount < 64, "No place left on the board")
  require(tail(head.coord).isEmpty, "Square already occupied")
  val isEmpty = false
}


