package fr.kalman.bliebchessrecorder.logic

import fastparse.all._

case class FenLexerError(msg: String)

object FenParser  {
  val pieceParser = P(CharIn("prnbqkPRNBQK"))
  val emptySqParser = P(CharIn('1' to '8'))
  val parseRank = (pieceParser | emptySqParser).rep(min=1,max=8).!.map(x => {
      val expandedFen = new StringBuilder(8)
      x.foreach(c => if (c.isDigit) expandedFen.append("." * c.asDigit) else expandedFen.append(c))

      expandedFen.toString()
    })
    .filter(x => x.length == 8)

  val activeColorParser = P(CharIn("wb")).!
  val castlingRightsParser = P( "-" | ("Q" | "K" | "q" | "k").rep(min=1, max=4)).!
    .filter(x => x.groupBy(identity).values.exists(_.length == 1))

  val AlgebricNotationParser = P(CharIn('a' to 'h') ~ CharIn("36")).!
  val enPassantTargetParser = P("-") | AlgebricNotationParser

  val positiveIntParser = P(CharIn('0' to '9').rep(1).!)
  val halfMoveParser = positiveIntParser
  val fullMoveParser = positiveIntParser.filter(x => x.toInt > 0)

  val FenStringParser =
    parseRank ~ P("/") ~ // Rank 8
    parseRank ~ P("/") ~ // Rank 7
    parseRank ~ P("/") ~ // Rank 6
    parseRank ~ P("/") ~ // Rank 5
    parseRank ~ P("/") ~ // Rank 4
    parseRank ~ P("/") ~ // Rank 3
    parseRank ~ P("/") ~ // Rank 2
    parseRank ~ P(" ") ~ // Rank 1
    activeColorParser ~ P(" ") ~
    castlingRightsParser ~ P(" ") ~
    enPassantTargetParser ~ P(" ") ~
    halfMoveParser ~ P(" ") ~
    fullMoveParser ~
    fastparse.all.End
}
