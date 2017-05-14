package fr.kalman.bliebchessrecorder.logic

import fr.kalman.bliebchessrecorder.logic.Color.Color

sealed trait Piece { val color: Color }

case class Pawn(color: Color) extends Piece
case class Rook( color: Color) extends Piece
case class Knight(color: Color) extends Piece
case class Bishop(color: Color) extends Piece
case class Queen(color: Color) extends Piece
case class King(color: Color) extends Piece
