
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

class UserActionTest extends FlatSpec with Matchers {

  "basic square tests" should "pass" in {
    Codpiece.squares.size shouldBe 64
    Codpiece.rankToChar(1) shouldBe '7'
    Codpiece.fileToChar(1) shouldBe 'b'
  }

  "basic coordinate tests" should "pass" in {
    val c = Codpiece.C(1,1)
    c + c shouldBe Codpiece.C(2,2)
    c * 3 shouldBe Codpiece.C(3,3)

    Codpiece.C(4,4).toSquare shouldBe Codpiece.e4
  }


  "mucking with the board" should "create correct material values" in {
    val b = Codpiece.startBoard.makeChild()

    b.material shouldBe 0

    b(Codpiece.a8) = Codpiece.empty

    b.material shouldBe Codpiece.wRook.value

    b.blackMaterial shouldBe b.whiteMaterial - Codpiece.wRook.value

    b(Codpiece.a8) = Codpiece.bRook

    b.material shouldBe 0
    b.blackMaterial shouldBe b.whiteMaterial

  }

  "move tables" should "be correct" in {
    val h2wp = Codpiece.whitePawnTable(Codpiece.h2)
    h2wp.captures.length shouldBe 1
    h2wp.doubles.length shouldBe 1
    h2wp.singles.length shouldBe 1
    h2wp.epMoves.length shouldBe 0

    val e2wp = Codpiece.whitePawnTable(Codpiece.e2)
    e2wp.captures.length shouldBe 2
    e2wp.doubles.length shouldBe 1
    e2wp.singles.length shouldBe 1
    e2wp.doubles(0).enPesantTarget shouldBe Codpiece.e3
    e2wp.epMoves.length shouldBe 0

    val e5wp = Codpiece.whitePawnTable(Codpiece.e5)
    e5wp.captures.length shouldBe 2
    e5wp.doubles.length shouldBe 0
    e5wp.singles.length shouldBe 1
    e5wp.epMoves.length shouldBe 2

    val e7wp = Codpiece.whitePawnTable(Codpiece.e7)
    e7wp.captures.length shouldBe 8
    e7wp.doubles.length shouldBe 0
    e7wp.singles.length shouldBe 4
    e7wp.epMoves.length shouldBe 0

    val h2bp = Codpiece.blackPawnTable(Codpiece.h2)
    h2bp.captures.length shouldBe 4
    h2bp.doubles.length shouldBe 0
    h2bp.singles.length shouldBe 4

    val e7bp = Codpiece.blackPawnTable(Codpiece.e7)
    e7bp.captures.length shouldBe 2
    e7bp.doubles.length shouldBe 1
    e7bp.singles.length shouldBe 1
  }

  "pawnMoves" should "work" in {
    val pdm = new Codpiece.PawnDoubleMove( Codpiece.e2, Codpiece.e4)
    pdm.enPesantTarget shouldBe Codpiece.e3

    val pep = new Codpiece.PawnEnPesant(Codpiece.C.fromSquare(Codpiece.e5),Codpiece.C.fromSquare(Codpiece.f6))
    pep.lift shouldBe Codpiece.f5
  }

  "movegen individual function numbers" should "be correct" in {
    val b = Codpiece.startBoard

    val queenMovesFrome4 = Codpiece.queenMoveGen(b, Codpiece.e4, 1)
    queenMovesFrome4.length shouldBe 19

    val rookMovesFromd4 = Codpiece.rookMoveGen(b, Codpiece.d4, 1)
    rookMovesFromd4.length shouldBe 11

    val bishopMovesFromc4 = Codpiece.rookMoveGen(b, Codpiece.c4, 1)
    bishopMovesFromc4.length shouldBe 11

    val knightMovesFrome5 = Codpiece.knightMoveGen(b, Codpiece.e5, 1)
    knightMovesFrome5.length shouldBe 8

    val knightMovesFrome4 = Codpiece.knightMoveGen(b, Codpiece.e4, 1)
    knightMovesFrome4.length shouldBe 6

    val kingMovesFrome4 = Codpiece.kingMoveGen(b, Codpiece.e4, 1)
    kingMovesFrome4.length shouldBe 8

    //king moves including castling
    val b2 = b.makeChild()
    b2(Codpiece.e2) = Codpiece.empty
    b2(Codpiece.g1) = Codpiece.empty
    Codpiece.freeAndClear(1,Codpiece.g1)(b2) shouldBe true
    Codpiece.freeAndClear(1,Codpiece.f1)(b2) shouldBe false
    Codpiece.freeAndClear(1,Codpiece.f1,Codpiece.g1)(b2) shouldBe false
    val castleMoves = Codpiece.whiteCastle(b2)
    castleMoves.length shouldBe 0

    b2(Codpiece.f1) = Codpiece.empty
    Codpiece.freeAndClear(1,Codpiece.g1)(b2) shouldBe true
    Codpiece.freeAndClear(1,Codpiece.f1)(b2) shouldBe true
    Codpiece.freeAndClear(1,Codpiece.f1,Codpiece.g1)(b2) shouldBe true
    val castle2Moves = Codpiece.whiteCastle(b2)
    castle2Moves.length shouldBe 1

    val kingMovesFrome1 = Codpiece.kingMoveGen(b2, Codpiece.e1, 1)
    kingMovesFrome1.length shouldBe 3

    val b3 = b2.makeChild()
    b3(Codpiece.h1) = Codpiece.empty
    b3.castlingRight.toList.length shouldBe 3

    val kingMovesFrome1RookGone = Codpiece.kingMoveGen(b3, Codpiece.e1, 1)
    kingMovesFrome1RookGone.length shouldBe 2

    val b4 = b2.makeChild()
    b2.castlingRight = Set() // pieces are in place, but castling rights destroyed
    val kingMovesFrome1CastlingWrecked = Codpiece.kingMoveGen(b2, Codpiece.e1, 1)
    kingMovesFrome1CastlingWrecked.length shouldBe 2

    val wpme2 = Codpiece.whitePawnGen(b,Codpiece.e2,1)
    wpme2.length shouldBe 2

    val wpme6 = Codpiece.whitePawnGen(b,Codpiece.e6,1)
    wpme2.length shouldBe 2

    val wpme7 = Codpiece.whitePawnGen(b,Codpiece.e7,1)
    wpme7.length shouldBe 8

    val wpmh7 = Codpiece.whitePawnGen(b,Codpiece.h7,1)
    wpmh7.length shouldBe 4
  }

  "movegen" should "accurately generate moves" in {
    val b = Codpiece.startBoard

    val moves = Codpiece.moveGen(b)

    moves.length shouldBe 20
  }

  def bench() = {
    val b = Codpiece.startBoard
    val st = System.currentTimeMillis()
    val m = new Codpiece.Move(48,48-16)
    var x = 0
    for( a <- 1 to 1000000) {
      //print(".")
      val b2 = Codpiece.play(b, m)
      x = x + b2.toMove
    }
    val et = System.currentTimeMillis()

    println(x)
    println("Time Taken:" + ( et-st) )
  }

  def perft( b:Codpiece.Board, depth:Int ):Int = {
    if (depth==0) 1 else
    {
      println(b)

       val moves = Codpiece.moveGen(b)
      println(moves)
       moves.map(m=>perft(Codpiece.play(b,m),depth-1) ).reduce( _ + _ )
    }
  }

  "perfttests" should "be accurate" in {
    val b = Codpiece.startBoard

    perft(b,1) shouldBe 20
    perft(b,2) shouldBe 400
    perft(b,3) shouldBe 8902
    perft(b,4) shouldBe 197281
    perft(b,5) shouldBe 4865609
    perft(b,6) shouldBe 119060324

  }
}