

import org.scalatest.Matchers
import org.scalatest.FlatSpec

import Codpiece._

class CodpieceTest extends FlatSpec with Matchers {

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


    val bpee4 = blackPawnTable(e4).epMoves
    bpee4.length shouldBe 2

    var v = pawnMoveToPromoters(Move(e7, e8))
    v.length shouldBe 4
    assert(v.forall(m => m.asInstanceOf[PromotionMove].promoter != null))
  }

  "sideattcks" should "be accurate" in {
    implicit val b = startBoard.makeChild()

    sideAttacks(1, e3) shouldBe true
    sideAttacks(1, e4) shouldBe false


  }

  "movegen individual function numbers" should "be correct" in {
    val b = startBoard.makeChild()

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
    val b2 = startBoard.makeChild()
    b2(e2) = Codpiece.empty
    b2(g1) = Codpiece.empty
    freeAndClear(-1, g1)(b2) shouldBe true
    freeAndClear(-1, f1)(b2) shouldBe false
    freeAndClear(-1, f1, g1)(b2) shouldBe false
    val castleMoves = whiteCastle(b2)
    castleMoves.length shouldBe 0

    b2(f1) = Codpiece.empty
    freeAndClear(-1, g1)(b2) shouldBe true
    freeAndClear(-1, f1)(b2) shouldBe true
    freeAndClear(-1, f1, g1)(b2) shouldBe true
    val castle2Moves = whiteCastle(b2)
    castle2Moves.length shouldBe 1

    //make sure we can't castle out of check
    val bKingAttacked = b.makeChild()
    bKingAttacked(f2) = bBishop
    kingToMoveInCheck(bKingAttacked) shouldBe true
    val castle3Moves = whiteCastle(bKingAttacked)
    castle3Moves.length shouldBe 0

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

    val pmoves = wPawn.movegen(b5, e2, 1)

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
    implicit val b = startBoard.makeChild().makeChild()

    b.toMove shouldBe 1

    canCaptureKing(b) shouldBe false
    kingToMoveInCheck(b) shouldBe false

    b(f2) = Codpiece.empty
    b(h4) = bQueen
    b.toMove shouldBe 1
    kingToMoveInCheck shouldBe true
    canCaptureKing shouldBe false

    b(f7) = Codpiece.empty
    b(h5) = wQueen
    kingToMoveInCheck shouldBe true
    canCaptureKing shouldBe true
  }

  "lastCaptureAt" should "be right" in {
    val b = startBoard

    b.lastCaptureAt shouldBe -1

    play(b, new Move(e2, e7)).lastCaptureAt shouldBe e7
    play(b, new Move(e2, e6)).lastCaptureAt shouldBe -1
  }

  def longToArray(l: Long) = squares.map(sq => if (((1L << sq) & l) != 0) 1 else 0)

  def countBits(l: Long) = longToArray(l).sum

  def printBitBoard(l: Long) = println(boardToString(longToArray(l).map(s => s.toString)))

  "basic pawn counting, etc" should "be accurate" in {
    val pe = PawnEval(startBoard)

    for (f <- 0 to 7) {
      assert(countBits(pe.whitePawnsInColumn(f)) == 1)
      assert(countBits(pe.blackPawnsInColumn(f)) == 1)
    }

    val board2 = play(play(startBoard, Move(g2, f3)), Move(b7, c6))

    val pe2 = PawnEval(board2)

    pe2.whitePawnsInColumn(6) shouldBe 0
    pe2.blackPawnsInColumn(1) shouldBe 0

    assert(pe2.whitePawnsInColumn(1) != 0)
    assert(pe2.blackPawnsInColumn(1) == 0)

    pe2.blackFileState(1) shouldBe semiOpen
    pe2.blackFileState(2) shouldBe closed

    pe2.whiteFileState(6) shouldBe semiOpen
    pe2.whiteFileState(5) shouldBe closed

    val board3 = play(play(board2, Move(g7, f6)), Move(b2, c3))
    val pe3 = PawnEval(board3)
    pe3.whiteFileState(6) shouldBe open
    pe3.blackFileState(1) shouldBe open
  }

  "passed pawn tests" should "be correct" in {
    val pe = PawnEval(startBoard)

    pe.whitePawnPassedAt(e2) shouldBe 0
    pe.whitePawnPassedAt(a2) shouldBe 0
    pe.whitePawnPassedAt(h2) shouldBe 0

    pe.whitePawnPassedAt(e2) shouldBe 0

    pe.whitePawnPassedAt(e6) shouldBe 0

    pe.whitePawnPassedAt(e7) > 0 shouldBe true
    pe.whitePawnPassedAt(a7) > 0 shouldBe true
    pe.whitePawnPassedAt(h7) > 0 shouldBe true

    pe.whitePawnPassedAt(e7) > 0 shouldBe true
    pe.whitePawnPassedAt(a7) > 0 shouldBe true
    pe.whitePawnPassedAt(h7) > 0 shouldBe true

    pe.blackPawnPassedAt(e2) > 0 shouldBe true
    pe.blackPawnPassedAt(a2) > 0 shouldBe true
    pe.blackPawnPassedAt(h2) > 0 shouldBe true

    pe.whitePawnPassedAt(e2) shouldBe 0
    pe.whitePawnPassedAt(a2) shouldBe 0
    pe.whitePawnPassedAt(h2) shouldBe 0

    pe.blackPawnPassedAt(e7) shouldBe 0
    pe.blackPawnPassedAt(a7) shouldBe 0
    pe.blackPawnPassedAt(h7) shouldBe 0

    pe.blackPawnPassedAt(e6) shouldBe 0

  }


  "passed pawn bonuses" should "work" in {
    val b = startBoard.makeChild()
    //println(boardToString(whitePassedPawnBonuses.map(i => i.toString)))
    //println(boardToString(blackPassedPawnBonuses.map(i => i.toString)))
  }

  def multiplay(b: Board, moves: (Int, Int)*) = moves.foldLeft(b)((b, tup) => play(b, Move(tup._1, tup._2)))

  /*def printBitBoard(l:Long) = {
    println(boardToString(squares.map(sq=>if ((l & (1 >>sq))!=0) "1" else "0")))
  }*/

  "sense isolated and doubled pawns" should "work" in {
    val b = startBoard.makeChild()
    val pe = PawnEval(b)

    //printBitBoard(pe.isolatedPawnPunch(0))

    //for (a <- (0 to 7))
    //  printBitBoard( pe.isolatedPawnPunch((a)))

    for (a <- (0 to 7)) {
      pe.whitePawnIsolatedOn(0) shouldBe 0
      pe.whitePawnDoubledOn(0) shouldBe 0

      pe.blackPawnIsolatedOn(0) shouldBe 0
      pe.blackPawnDoubledOn(0) shouldBe 0
    }

    val b3 = multiplay(b, (g2, f3), (g7, f6))

    val pe2 = PawnEval(b3)
    pe2.whitePawnIsolatedOn(7) < 0 shouldBe true
    pe2.blackPawnIsolatedOn(7) < 0 shouldBe true

    pe2.whitePawnDoubledOn(5) < 0 shouldBe true
    pe2.blackPawnDoubledOn(5) < 0 shouldBe true

  }

  "rooks" should "like the seventh rank and open files" in {
    wRook.simpleEval(e2) shouldBe 0
    wRook.simpleEval(a5) shouldBe 0
    wRook.simpleEval(e7) shouldBe RookSeventhBonus
    wRook.simpleEval(a7) shouldBe RookSeventhBonus

    bRook.simpleEval(e2) shouldBe RookSeventhBonus
    bRook.simpleEval(a2) shouldBe RookSeventhBonus
    bRook.simpleEval(a5) shouldBe 0
    bRook.simpleEval(e7) shouldBe 0


    val b = startBoard.makeChild()
    val pe = PawnEval(b)

    scoreWhiteRook(h1, b, pe) shouldBe 0
    scoreBlackRook(h1, b, pe) shouldBe 0

    b(h2) = Codpiece.empty

    val pe2 = PawnEval(b)
    scoreWhiteRook(h1, b, pe2) shouldBe 30
    scoreBlackRook(h1, b, pe2) shouldBe 0

    b(h7) = Codpiece.empty
    val pe3 = PawnEval(b)
    scoreWhiteRook(h1, b, pe3) shouldBe 60
    scoreBlackRook(h1, b, pe3) shouldBe 60
  }

  "king safety" should "behave" in {
    val kingSafetyOpeningMap = squares.map(kingSafety(_, fullArmyValue.toInt, null))
    println(boardToString(kingSafetyOpeningMap.map(s => s.toString)))

    val kingSafetyEndingMap = squares.map(kingSafety(_, 600, null))
    println(boardToString(kingSafetyEndingMap.map(s => s.toString)))

    val kingSafetyMidMap = squares.map(kingSafety(_, fullArmyValue.toInt / 2, null))
    println(boardToString(kingSafetyMidMap.map(s => s.toString)))

    //kingSafety(g1, fullArmyValue.toInt, null) shouldBe 50
    //kingSafety(e4, fullArmyValue.toInt, null) shouldBe -150
  }

  //TODO: add tests to see if it plays reasonable moves

  def searchTest(b: Board, depth: Int, moves: Move*): Boolean = {
    val (score, move) = search(b, depth)
    moves.contains(move)
  }

  "search" should "produce reasonable moves" in {
    val b = startBoard.makeChild()

    searchTest(b, 4, Move(e2, e4), Move(d2,d4)) shouldBe true
  }

  def bench() = {
    val b = startBoard
    val st = System.currentTimeMillis()
    val m = new Move(e2, e4)
    var x = 0
    for (a <- 1 to 1000000) {
      val b2 = play(b, m)
      x = x + b2.toMove
      //canCaptureKing(b2)
      sideAttacks(1, 1)(b2)
    }
    val et = System.currentTimeMillis()

    println(x)
    println("Time Taken:" + (et - st))
  }


  def completeEval(board: Board) = {
    var e: Int = board.material
    for (sq <- squares) {
      val p = board(sq)
      e += p.simpleEval(sq)
    }
    e
  }

  def board_assertions(board: Board) = {
    var material: Int = 0
    var blackMaterial: Int = 0
    var whiteMaterial: Int = 0
    var hash: Long = 0L
    var pawnHash: Long = 0L
    var whitePawnCount = 0
    var blackPawnCount = 0
    var whitePawnMap = 0L
    var blackPawnMap = 0L

    for (sq <- squares) {
      val piece = board(sq)
      material += piece.value
      if (piece.value < 0) blackMaterial -= piece.value
      if (piece.value > 0) whiteMaterial += piece.value
      val ph = piece.hashes(sq)
      if (Math.abs(piece.value) == 100) pawnHash = pawnHash ^ ph
      hash = hash ^ ph
      if (piece == wPawn) {
        whitePawnCount = whitePawnCount + 1
        whitePawnMap = whitePawnMap | bitPawn(sq)
      }
      if (piece == bPawn) {
        blackPawnCount = blackPawnCount + 1
        blackPawnMap = blackPawnMap | bitPawn(sq)
      }
    }

    board.material shouldBe material
    board.whiteMaterial shouldBe whiteMaterial
    board.blackMaterial shouldBe blackMaterial
    board.hash shouldBe hash
    board.pawnHash shouldBe pawnHash
    board.simpleEval shouldBe completeEval(board)

    board.blackPawnCount shouldBe blackPawnCount
    board.whitePawnCount shouldBe whitePawnCount

    board.blackPawnMap shouldBe blackPawnMap
    board.whitePawnMap shouldBe whitePawnMap
  }

  def perft(implicit b: Board, depth: Int): Int = {
    //board_assertions(b)
    if (depth == 0) 1
    else {
      val moves = moveGen(b)
      val moves2 = moveGen(b)
      moves.map(m => {
        val b2 = play(b, m)
        if (canCaptureKing(b2))
          0
        else perft(play(b, m), depth - 1)
      }).sum
    }
  }

  def ftime = System.currentTimeMillis() / 1000.0

  def profile[R](repeat: Int)(code: => R) = {
    (1 to 1000).foreach(i => code) // warmup
    val start = System.nanoTime()
    (1 to repeat).foreach(i => code)
    1.0 / (((System.nanoTime - start) / repeat) / 1000000000.0)
  }

  def bench_movegen = {
    //start: 34568/sec
    val kiwiBoard = Codpiece.fromFEN(kiwi)
    println("Movegen time:" + (profile(100000) {
      moveGen(kiwiBoard)
    }))
  }

  def bench_play = {
    //start 91441/sec
    val kiwiBoard = Codpiece.fromFEN(kiwi)
    val moves = moveGen(kiwiBoard)
    println("play time:" + (profile(100000) {
      for (move <- moves) play(kiwiBoard, move)
    }))
  }

  def perft2(implicit b: Board, depth: Int): Int = {
    val start = ftime
    val count = perft(b, depth)
    val end = ftime
    println("perft seconds:" + (end - start))
    count
  }


  val findPromotionBugs = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1"

  val kiwi = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  val fenPos5 = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"

  "perfttests" should "be accurate" in {

    val fpb = Codpiece.fromFEN(findPromotionBugs)
    perft(fpb, 1) shouldBe 24
    perft(fpb, 2) shouldBe 496
    perft(fpb, 3) shouldBe 9483
    perft(fpb, 4) shouldBe 182838
    //perft(fpb, 5) shouldBe 3605103

    val b = startBoard

    perft(b, 1) shouldBe 20
    perft(b, 2) shouldBe 400
    perft(b, 3) shouldBe 8902
    perft(b, 4) shouldBe 197281 //shows king evading capture
    //perft2(b, 5) shouldBe 4865609
    //perft(b, 6) shouldBe 119060324

    val kiwiBoard = Codpiece.fromFEN(kiwi)

    perft(kiwiBoard, 1) shouldBe 48
    perft(kiwiBoard, 2) shouldBe 2039
    perft(kiwiBoard, 3) shouldBe 97862
    //perft(kiwiBoard, 4) shouldBe 4085603
    //perft(kiwiBoard, 5) shouldBe 193690690

    val pos5 = Codpiece.fromFEN(fenPos5)
    perft(pos5, 1) shouldBe 44
    perft(pos5, 2) shouldBe 1486
    perft(pos5, 3) shouldBe 62379

    bench_movegen
    bench_play
  }
}