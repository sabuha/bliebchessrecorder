package fr.kalman.bliebchessrecorder.logic

import fr.kalman.bliebchessrecorder.logic.Color._
import scala.collection.immutable.SortedSet

sealed abstract class Piece {
  private val authorizedId = SortedSet('p','r','n', 'b', 'q', 'k')
  require(authorizedId contains id)

  // Implement id as lazy or the requirement above will fail, because it is not set yet (=0)
  val id: Char
  val color: Color

  override val toString: String = if (color == WHITE) id.toUpper.toString else id.toString
  override val hashCode: Int = (1 + authorizedId.toVector.indexOf(id)) * (if (color==BLACK) 2 else 1)
}

case class Pawn(color: Color) extends Piece { lazy val id='p' }
case class Rook(color: Color) extends Piece { lazy val id='r' }
case class Knight(color: Color) extends Piece { lazy val id='n' }
case class Bishop(color: Color) extends Piece { lazy val id='b' }
case class Queen(color: Color) extends Piece { lazy val id='q' }
case class King(color: Color) extends Piece { lazy val id='k' }

