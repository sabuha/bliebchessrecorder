import fr.kalman.bliebchessrecorder.logic.FenParser
import fastparse.core.Parsed
import fastparse.all._

class FenParserSpec extends org.scalatest.FreeSpec {
  def parsingSuccess(p:Parsed[_,_,_]): Boolean = {
    p.fold(
      (_,_,_) => false,
      (_,_) => true
    )
  }

  "A FEN String" - {
    "has a field for piece placement" - {
      val p = FenParser.parseRank ~ fastparse.all.End
      "a rank is described by lower case for black, upper case for white and digits for empty squares" in {
        assert(parsingSuccess(p.parse("rbnqkbnr")))
        assert(parsingSuccess(p.parse("rbnqkbn1")))
        assert(parsingSuccess(p.parse("8")))
        assert(parsingSuccess(p.parse("1Pp3Qn")))
      }
      "each rank must expand to 8 squares, and letters must be valid identifiers" in {
        assert(!parsingSuccess(p.parse("rbnqkbnX")))
        assert(!parsingSuccess(p.parse("rbnqkbn2")))
        assert(!parsingSuccess(p.parse("9")))
        assert(!parsingSuccess(p.parse("1Pp3Q")))
      }
    }
    "has a castling availability field" - {
      val p = P(FenParser.castlingRightsParser ~ fastparse.all.End)
      "which cannot be null" in {
        assert(!parsingSuccess(p.parse("")))
      }
      "equals '-' if no castling right" in {
        assert(parsingSuccess(p.parse("-")))
      }
      "equals KQkq (in any order) if all castling rights are available" in {
        assert(parsingSuccess(p.parse("KQkq")))
        assert(parsingSuccess(p.parse("KkQq")))
        assert(parsingSuccess(p.parse("KkqQ")))
        assert(parsingSuccess(p.parse("KqQk")))
        assert(parsingSuccess(p.parse("qkKQ")))
      }
      "cannot give a castling right twice" in {
        assert(!parsingSuccess(p.parse("qq")))
      }
      "cannot mix castling rights with no right" in {
        assert(!parsingSuccess(p.parse("-q")))
        assert(!parsingSuccess(p.parse("q-")))
      }
    }
    "has a en passant target square field" - {
      val p = FenParser.enPassantTargetParser ~ fastparse.all.End
      "which can be '-' if no target square" in {
        assert(parsingSuccess(p.parse("-")))
      }
      "which is never empty" in {
        assert(!parsingSuccess(p.parse("")))
      }
      "which describes the position in algebric notation" in {
        assert(parsingSuccess(p.parse("e3")))
        assert(parsingSuccess(p.parse("e6")))
      }
      "which concerns only rank 3 or 6 by definition" in {
        assert(!parsingSuccess(p.parse("e4")))
        assert(!parsingSuccess(p.parse("e5")))
      }
    }
    "has a halfmove field" - {
      val p = FenParser.halfMoveParser ~ fastparse.all.End
      "which can be 0 at minimum" in {
        assert(parsingSuccess(p.parse("0")))
      }
      "or any integer above zero" in {
        assert(parsingSuccess(p.parse("1")))
        assert(parsingSuccess(p.parse("12")))
        assert(parsingSuccess(p.parse("00013")))
      }
    }
    "has a fullmove field" - {
      val p = FenParser.fullMoveParser ~ fastparse.all.End
      "which can be 1 at minimum" in {
        assert(!parsingSuccess(p.parse("0")))
        assert(parsingSuccess(p.parse("1")))
      }
      "or any integer above one" in {
        assert(parsingSuccess(p.parse("2")))
        assert(parsingSuccess(p.parse("12")))
        assert(parsingSuccess(p.parse("00013")))
      }
    }
  }

  "Some valid FEN Strings" in {
    val p = FenParser.FenStringParser
    val fenList = List(
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
      "1qr3k1/p2nbppp/bp2p3/3p4/3P4/1P2PNP1/P2Q1PBP/1N2R1K1 b - - 0 12",
      "1r2r1k1/3bnppp/p2q4/2RPp3/4P3/6P1/2Q1NPBP/2R3K1 w - - 0 15",
      "2b1k2r/2p2ppp/1qp4n/7B/1p2P3/5Q2/PPPr2PP/R2N1R1K b k - 0 32",
      "2b5/1p4k1/p2R2P1/4Np2/1P3Pp1/1r6/5K2/8 w - - 12 5",
      "2brr1k1/ppq2ppp/2pb1n2/8/3NP3/2P2P2/P1Q2BPP/1R1R1BK1 w - - 14 40",
      "2kr2nr/1pp3pp/p1pb4/4p2b/4P1P1/5N1P/PPPN1P2/R1B1R1K1 b - - 5 52")

    fenList.foreach(x => assert(parsingSuccess(p.parse(x))))
  }
  "Invalid FEN sample" in {
    val p = FenParser.FenStringParser
    val fenList = List(
      "rnbqkbn/rpppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
      "1qr3k1/p2nbppp/bp2p3/3p4/3P4/1P2PNP1/P2Q2PBP/1N2R1K1 b - - 0 12",
      "1r2r1k1/3bnppp/p2q4/2RPp3/4P3/6P1/2Q1NPBP2R3K1 w - - 0 15",
      "2b1k2r/2p2ppp/1qp4n/7B/1p2P3/5Q2/PPPr2PP/R2N1R1K b kk - 0 32",
      "2b5/1p4k1/p2R2P1/4Np2/1P3Pp1/1r6/5K2/8 w - 12 5",
      "2brr1k1/ppq2ppp/2pb1n2/8/3NP3/2P2P2/P1Q2BPP/1R1R1BK1 w - A3 14 0",
      "2kr2nr/1pp3pp/p1pb4/4p2b/4P1P1/5N1P/PPPN1P2/R1B1R1K1 b - - 5 52d",
      "2r1k2r/1p1qbppp/p3pn2/3pBb2/3P4/1QN1P3/PP2BPPP/2R2RK1 b k - 0f 2",
      "rnq1nrk1/pp3pbp/6p1/3p4/3P4/5N2/PP2BPPP/R1BQK2R x KQ - 0 1"
    )
    fenList.foreach(x => assert(!parsingSuccess(p.parse(x))))
  }
}
