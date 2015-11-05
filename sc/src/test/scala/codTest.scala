

import org.scalatest.Matchers
import org.scalatest.FlatSpec

import Codpiece._

class UserActionTest extends FlatSpec with Matchers {

  "basic square tests" should "pass" in {
    squares.size shouldBe 64
    rankToChar(1) shouldBe '7'
    fileToChar(1) shouldBe 'b'
  }

  "basic coordinate tests" should "pass" in {
    val c = C(1, 1)
    c + c shouldBe C(2, 2)
    c * 3 shouldBe C(3, 3)

    C(4, 4).toSquare shouldBe e4
  }


  "mucking with the board" should "create correct material values" in {
    val b = startBoard.makeChild()
    b.material shouldBe 0
    b(a8) = Codpiece.empty
    b.material shouldBe wRook.value
    b.blackMaterial shouldBe b.whiteMaterial - wRook.value
    b(a8) = bRook
    b.material shouldBe 0
    b.blackMaterial shouldBe b.whiteMaterial

  }

  "king location" should "be tracked" in {
    val b = startBoard.makeChild()
    b.whiteKingAt shouldBe e1
    b.blackKingAt shouldBe e8

    val m1 = new Move(e1, e2)
    val b1 = play(b, m1)
    b1.whiteKingAt shouldBe e2

    val m2 = new Move(e8, e4)
    val b2 = play(b, m2)
    b2.blackKingAt shouldBe e4
  }

  "move tables" should "be correct" in {
    val h2wp = whitePawnTable(h2)
    h2wp.captures.length shouldBe 1
    h2wp.doubles.length shouldBe 1
    h2wp.singles.length shouldBe 1
    h2wp.epMoves.length shouldBe 0

    val e2wp = whitePawnTable(e2)
    e2wp.captures.length shouldBe 2
    e2wp.doubles.length shouldBe 1
    e2wp.singles.length shouldBe 1
    e2wp.doubles(0).enPesantTarget shouldBe e3
    e2wp.epMoves.length shouldBe 0

    val e5wp = whitePawnTable(e5)
    e5wp.captures.length shouldBe 2
    e5wp.doubles.length shouldBe 0
    e5wp.singles.length shouldBe 1
    e5wp.epMoves.length shouldBe 2

    val e7wp = whitePawnTable(e7)
    e7wp.captures.length shouldBe 8
    e7wp.doubles.length shouldBe 0
    e7wp.singles.length shouldBe 4
    e7wp.epMoves.length shouldBe 0

    val h2bp = blackPawnTable(h2)
    h2bp.captures.length shouldBe 4
    h2bp.doubles.length shouldBe 0
    h2bp.singles.length shouldBe 4
    h2bp.epMoves.length shouldBe 0

    val e7bp = blackPawnTable(e7)
    e7bp.captures.length shouldBe 2
    e7bp.doubles.length shouldBe 1
    e7bp.singles.length shouldBe 1
  }

  "special pawn Moves" should "work" in {
    val pdm = new PawnDoubleMove(e2, e4)
    pdm.enPesantTarget shouldBe e3

    val pep = new PawnEnPesant(e5, f6)
    pep.lift shouldBe f5
    pep.from shouldBe e5
    pep.to shouldBe f6

    val wpee5 = whitePawnTable(e5).epMoves
    wpee5.length shouldBe 2
    //println( "en pesants from e5 : " + wpee5 )

    val bpee4 = blackPawnTable(e4).epMoves
    bpee4.length shouldBe 2
    //println( "en pesants from e4 : " + bpee4 )
  }


  "movegen individual function numbers" should "be correct" in {
    val b = startBoard

    val queenMovesFrome4 = queenMoveGen(b, e4, 1)
    queenMovesFrome4.length shouldBe 19

    val rookMovesFromd4 = rookMoveGen(b, d4, 1)
    rookMovesFromd4.length shouldBe 11

    val bishopMovesFromc4 = rookMoveGen(b, c4, 1)
    bishopMovesFromc4.length shouldBe 11

    val knightMovesFrome5 = knightMoveGen(b, e5, 1)
    knightMovesFrome5.length shouldBe 8

    val knightMovesFrome4 = knightMoveGen(b, e4, 1)
    knightMovesFrome4.length shouldBe 6

    val kingMovesFrome4 = kingMoveGen(b, e4, 1)
    kingMovesFrome4.length shouldBe 8

    //king moves including castling
    val b2 = b.makeChild()
    b2(e2) = Codpiece.empty
    b2(g1) = Codpiece.empty
    freeAndClear(1, g1)(b2) shouldBe true
    freeAndClear(1, f1)(b2) shouldBe false
    freeAndClear(1, f1, g1)(b2) shouldBe false
    val castleMoves = whiteCastle(b2)
    castleMoves.length shouldBe 0

    b2(f1) = Codpiece.empty
    freeAndClear(1, g1)(b2) shouldBe true
    freeAndClear(1, f1)(b2) shouldBe true
    freeAndClear(1, f1, g1)(b2) shouldBe true
    val castle2Moves = whiteCastle(b2)
    castle2Moves.length shouldBe 1

    val kingMovesFrome1 = kingMoveGen(b2, e1, 1)
    kingMovesFrome1.length shouldBe 3

    val b3 = b2.makeChild()
    b3(h1) = Codpiece.empty
    b3.castlingRight.toList.length shouldBe 3

    val kingMovesFrome1RookGone = kingMoveGen(b3, e1, 1)
    kingMovesFrome1RookGone.length shouldBe 2

    val b4 = b2.makeChild()
    b2.castlingRight = Set() // pieces are in place, but castling rights destroyed
    val kingMovesFrome1CastlingWrecked = kingMoveGen(b2, e1, 1)
    kingMovesFrome1CastlingWrecked.length shouldBe 2

    val wpme2 = whitePawnGen(b, e2, 1)
    wpme2.length shouldBe 2

    val wpme6 = whitePawnGen(b, e6, 1)
    wpme2.length shouldBe 2

    val wpme7 = whitePawnGen(b, e7, 1)
    wpme7.length shouldBe 8

    val wpmh7 = whitePawnGen(b, h7, 1)
    wpmh7.length shouldBe 4

    val b5 = b.makeChild()
    b5(e4) = bPawn
    b5(f3) = wPawn
    println(b5)
    val pmoves = wPawn.movegen(b5, e2, 1)
    println(pmoves)
    pmoves.length shouldBe 1
    val pMoves2 = wPawn.movegen(b5, d2, 1)
    pMoves2.length shouldBe 2
    val pMoves3 = wPawn.movegen(b5, f2, 1)
    pMoves3.length shouldBe 0
  }

  "movegen" should "accurately generate moves" in {
    val b = startBoard
    val moves = moveGen(b)
    moves.length shouldBe 20
  }

  "en pesant moves" should "get generated and play right" in {
    val b = startBoard.makeChild()

    val b1 = play(b, new Move(e7, e4))
    val b2 = play(b1, new PawnDoubleMove(d2, d4))
    b2.ep_target shouldBe d3

    val e4ep = blackPawnTable(e4).epMoves
    e4ep.length shouldBe 2

    e4ep.filter(m => m.lift == d4).length shouldBe 1

    val allmoves = moveGen(b2)

    val moves = moveGen(b2).filter(m => m.to == d3)
    moves.length shouldBe 1 //the one en pesant move
    val m = moves(0)

    val b3 = play(b2, m)
    b3(e4) shouldBe Codpiece.empty
    b3(d3) shouldBe bPawn
    b3(d4) shouldBe Codpiece.empty

  }


  "kingSquareAttcks" should "work" in {
    implicit val b = startBoard

    kingAttacks(e2, wKing) shouldBe true
    kingAttacks(e3, wKing) shouldBe false

    kingAttacks(e7, bKing) shouldBe true
    kingAttacks(e6, bKing) shouldBe false
  }

  "knightSquareAttcks" should "work" in {
    implicit val b = startBoard
    knightAttacks(e2, wKnight) shouldBe true
    knightAttacks(f3, wKnight) shouldBe true
    knightAttacks(e3, wKnight) shouldBe false

    knightAttacks(e7, bKnight) shouldBe true
    knightAttacks(f6, bKnight) shouldBe true
    knightAttacks(e6, bKnight) shouldBe false
  }

  "pawnAttacks" should "work" in {
    implicit val b = startBoard
    pawnAttacks(e3, wPawn) shouldBe true
    pawnAttacks(e1, wPawn) shouldBe false

    pawnAttacks(e6, bPawn) shouldBe true
    pawnAttacks(e8, bPawn) shouldBe false
  }

  "scanrays" should "determine ray attacks" in {
    implicit val b = startBoard

    scanRays(e2, bishopMoveLookup, List(wBishop, wQueen)) shouldBe true

  }

  "kingAttacked" should "work" in {
    implicit val b = startBoard.makeChild()
    kingIsInDanger(1) shouldBe false
    kingIsInDanger(-1) shouldBe false

    b(f2) = Codpiece.empty
    b(h4) = bQueen
    kingIsInDanger(1) shouldBe true

    b(f7) = Codpiece.empty
    b(h5) = wQueen
    kingIsInDanger(-1) shouldBe true
  }

  def bench() = {
    val b = startBoard
    val st = System.currentTimeMillis()
    val m = new Move(e2, e4)
    var x = 0
    for (a <- 1 to 1000000) {
      val b2 = play(b, m)
      x = x + b2.toMove
    }
    val et = System.currentTimeMillis()

    println(x)
    println("Time Taken:" + (et - st))
  }

  def perft(implicit b: Board, depth: Int): Int = {
    if (b.toMove == 1 && kingIsInDanger(-1)) return 0
    if (b.toMove == -1 && kingIsInDanger(1)) return 0
    //b.material shouldBe b.squares.map(_.value).reduce(_+_)
    if (depth == 0) 1
    else {
      //println(b)
      val moves = moveGen(b)
      //println(moves)
      moves.map(m => {
        val b2 = play(b, m)
        if (canCaptureKing(b2)) 0 else perft(play(b, m), depth - 1)
      }).reduce(_ + _)
    }
  }

  "perfttests" should "be accurate" in {
    val b = startBoard

    perft(b, 1) shouldBe 20
    perft(b, 2) shouldBe 400
    perft(b, 3) shouldBe 8902
    perft(b, 4) shouldBe 197281 //shows king evading capture
    //perft(b, 5) shouldBe 4865609
    //perft(b, 6) shouldBe 119060324

    var b2 = Codpiece.fromFEN("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")

    println(moveGen(b2))
    perft(b2, 1) shouldBe 48
    perft(b2, 2) shouldBe 2039
    perft(b2, 3) shouldBe 97862
    perft(b2, 4) shouldBe 4085603
    perft(b2, 5) shouldBe 193690690

  }
}