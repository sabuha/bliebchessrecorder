package fr.kalman.bliebchessrecorder.logic

import language.implicitConversions
import fr.kalman.bliebchessrecorder.logic.SquareCoord.isOnBoard

object SquareCoord {
  private def isOnBoard(file: Int, rank: Int) = (file >= 0) && (file <= 7) && (rank >= 0) && (rank <= 7)

  /**
    * Implicit method to get a SquareCoord from a coordinate string
    * @param coord: a lowercase 2 char coordinates (eg "e4")
    * @return a SquareCoord instance
    * @example '''"a1".index''' is same as '''SquareCoord(0,0).index'''
    */
  implicit def stringToSquareCoord(coord: String) : SquareCoord = {
    require(coord.length == 2)
    require(('a' to 'h') contains coord(0))
    require(('1' to '8') contains coord(1))

    SquareCoord(coord(0)-'a', coord(1)-'1')
  }
}

/**
  * Represents square coordinate
  * @param file zero-based file number (0 to 7)
  * @param rank zero-based rank number (0 to 7)
  *
  * @example implicit conversion from string allows the following:
  * '''"val sq:SquareCoord = "h8"''' is same as '''val sq=SquareCoord(7,7)'''
  */
final case class SquareCoord(file:Int, rank:Int) {
  require (isOnBoard(file,rank), s"invalid coordinates $file:$rank")

  /**
    * Each square has a unique index between 0 and 63
    *
    * a1 = 0
    * h1 = 7
    * h8 = 63
    */

  lazy val index : Int = hashCode
  override val hashCode : Int = rank*8 + file

  def relativeSquare(fileOffset: Int, rankOffset:Int)
  : Option[SquareCoord] = (file+fileOffset, rank+rankOffset) match {
    case (newFile,newRank) if isOnBoard(newFile, newRank) => Some(SquareCoord(newFile, newRank))
    case _ => None
  }

  /**
    * Returns square coordinates
    * @return coordinate notation as a String of length 2 (e.g. "f3")
    */
  override def toString = s"${('a' + file).toChar}${('1' + rank).toChar}"
}


