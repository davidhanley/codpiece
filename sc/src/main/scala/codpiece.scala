

import com.sun.xml.internal.bind.v2.TODO

import scala.collection.{SortedSet, immutable}
import scala.collection.mutable.HashMap
import scala.ref.WeakReference
import scala.util._
import scala.util.control.Breaks._

object Codpiece {

  //TODOS:
  //proper mate-handling code
  //tree pruning for reduced memory use
  //pawn structure & passed pawns

  val squares = 0 to 63

  val (a8, b8, c8, d8, e8, f8, g8, h8) = (0, 1, 2, 3, 4, 5, 6, 7)
  val (a7, b7, c7, d7, e7, f7, g7, h7) = (8, 9, 10, 11, 12, 13, 14, 15)
  val (a6, b6, c6, d6, e6, f6, g6, h6) = (16, 17, 18, 19, 20, 21, 22, 23)
  val (a5, b5, c5, d5, e5, f5, g5, h5) = (24, 25, 26, 27, 28, 29, 30, 31)
  val (a4, b4, c4, d4, e4, f4, g4, h4) = (32, 33, 34, 35, 36, 37, 38, 39)
  val (a3, b3, c3, d3, e3, f3, g3, h3) = (40, 41, 42, 43, 44, 45, 46, 47)
  val (a2, b2, c2, d2, e2, f2, g2, h2) = (48, 49, 50, 51, 52, 53, 54, 55)
  val (a1, b1, c1, d1, e1, f1, g1, h1) = (56, 57, 58, 59, 60, 61, 62, 63)

  def rankToChar(r: Int) = "87654321"(r)

  def fileToChar(f: Int) = "abcdefgh"(f)

  def getRank(sq: Int) = sq / 8

  def getFile(sq: Int) = sq % 8

  def squareToString(sq: Int) = "" + fileToChar(getFile(sq)) + rankToChar(getRank(sq))

  val r = scala.util.Random

  case class Move(from: Int, to: Int) {
    def play(board: Board): Unit = {
      board(to) = board(from)
      board(from) = empty
    }

    def toStrings(): Seq[String] = {
      Seq(squareToString(from) + squareToString(to))
    }

    override def toString() = {
      toStrings()(0)
    }
  }

  case class C(r: Int, f: Int) {
    def +(that: C) = C(r + that.r, f + that.f)

    def *(mult: Int) = C(r * mult, f * mult)

    def toSquare = r * 8 + f

    def is_valid() = r >= 0 && r < 8 && f >= 0 && f < 8
  }

  object C {
    def fromSquare(sq: Int) = C(sq / 8, sq % 8)
  }

  val coords = squares.map(C.fromSquare(_))

  def coordsToMove(from: C, to: C) = new Move(from.toSquare, to.toSquare)

  def genHopper(source: C, deltas: Seq[C]) =
    deltas.map(source + _).filter(_.is_valid).map(coordsToMove(source, _))

  val knightDeltas = Array(C(-2, 1), C(-2, -1), C(-1, -2), C(-1, 2), C(1, -2), C(1, 2), C(2, -1), C(2, 1))
  val kingDeltas = Array(C(-1, -1), C(-1, 0), C(-1, 1), C(0, -1), C(0, 1), C(1, -1), C(1, 0), C(1, 1))

  val knightMoveLookup = coords.map(genHopper(_, knightDeltas))
  val kingMoveLookup = coords.map(genHopper(_, kingDeltas))

  def hopperGen(lookup: Seq[Seq[Move]])(b: Board, sq: Int, forSide: Int) =
    lookup(sq).filter((mv: Move) => b(mv.to).side != forSide)

  def knightMoveGen = hopperGen(knightMoveLookup) _

  class CastlingMove(from: Int, to: Int, rookFrom: Int, rookTo: Int) extends Move(from, to) {
    override def play(b: Board): Unit = {
      super.play(b)
      b(rookTo) = b(rookFrom)
      b(rookFrom) = empty
    }
  }

  val whiteKingsideCastle = new CastlingMove(e1, g1, h1, f1)
  val whiteQueensideCastle = new CastlingMove(e1, c1, a1, d1)
  val blackKingsideCastle = new CastlingMove(e8, g8, h8, f8)
  val blackQueensideCastle = new CastlingMove(e8, c8, a8, d8)

  def kingAttacks(sq: Int, king: Piece)(implicit b: Board) = kingMoveLookup(sq).exists(m => b(m.to) == king)

  def knightAttacks(sq: Int, knight: Piece)(implicit b: Board) = knightMoveLookup(sq).exists(m => b(m.to) == knight)

  def pawnAttacks(sq: Int, pawn: Piece)(implicit b: Board) = {
    val table = if (pawn == bPawn) whitePawnTable else blackPawnTable
    val capts = table(sq).captures
    //print(sq)
    //print(capts)
    capts.exists(m => b(m.to) == pawn)
  }

  def scanRay(mr: Seq[Move], pcs: List[Piece])(implicit b: Board): Boolean = {
    mr.foreach(mv => {
      val p = b(mv.to)
      if (p != empty) return (pcs.contains(p))
    })
    return false
  }

  def scanRays(sq: Int, mt: IndexedSeq[Seq[immutable.IndexedSeq[Move]]], pcs: List[Piece])(implicit b: Board) = {
    mt(sq).exists(ray => scanRay(ray, pcs))
  }

  def sideAttacks(side: Int, sq: Int)(implicit board: Board) = {
    val (king, knight, pawn, hsliders, vsliders) =
      if (side == 1) (wKing, wKnight, wPawn, List(wRook, wQueen), List(wBishop, wQueen))
      else
        (bKing, bKnight, bPawn, List(bRook, bQueen), List(bBishop, bQueen))
    kingAttacks(sq, king) || knightAttacks(sq, knight) || pawnAttacks(sq, pawn) ||
      scanRays(sq, rookMoveLookup, hsliders) || scanRays(sq, bishopMoveLookup, vsliders)
  }

  def king(side: Int)(implicit b: Board) = if (side == 1) b.whiteKingAt else b.blackKingAt

  def canCaptureKing(implicit b: Board) = sideAttacks(b.toMove, king(-b.toMove))

  def kingToMoveInCheck(implicit b: Board) = sideAttacks(-b.toMove, king(b.toMove))

  def freeAndClear(side: Int, squares: Int*)(implicit board: Board): Boolean =
    squares.forall(board(_) == empty) && squares.forall(sideAttacks(side, _) == false)

  //TODO: this is kinda gross. Fix it
  def whiteCastle(implicit b: Board): Seq[Move] = {
    lazy val king_not_attacked = kingToMoveInCheck == false
    val wkc = if (b.castlingRight('K') && freeAndClear(-1, f1, g1) && king_not_attacked) Some(whiteKingsideCastle) else None
    val wqc = if (b.castlingRight('Q') && freeAndClear(-1, d1, c1) && b(b1) == empty && king_not_attacked) Some(whiteQueensideCastle) else None
    List(wkc, wqc).flatMap(f => f)
  }


  //todo: optimize the following. A lot.
  def blackCastle(implicit b: Board): Seq[Move] = {
    lazy val king_not_attacked = kingToMoveInCheck == false
    val bkc = if (b.castlingRight('k') && freeAndClear(1, f8, g8) && king_not_attacked) Some(blackKingsideCastle) else None
    val bqc = if (b.castlingRight('q') && freeAndClear(1, d8, c8) && b(b8) == empty && king_not_attacked) Some(blackQueensideCastle) else None
    List(bkc, bqc).flatMap(f => f)
  }

  def kingMoveGen(b: Board, sq: Int, forSide: Int) = {
    val basicKingMoves = hopperGen(kingMoveLookup)(b, sq, forSide)
    if (sq == e1 && forSide == 1) basicKingMoves ++ whiteCastle(b)
    else if (sq == e8 && forSide == -1) basicKingMoves ++ blackCastle(b)
    else basicKingMoves
  }

  def genSlideRay(source: C, delta: C) =
    (1 to 8).map(source + delta * _).filter(_.is_valid).map(coordsToMove(source, _))

  def genSlideRays(source: C, deltas: Seq[C]) = deltas.map(genSlideRay(source, _))

  val bishopDeltas = Array(C(-1, -1), C(-1, 1), C(1, -1), C(1, 1))
  val rookDeltas = Array(C(-1, 0), C(1, 0), C(0, -1), C(0, 1))

  val bishopMoveLookup = coords.map(genSlideRays(_, bishopDeltas))
  val rookMoveLookup = coords.map(genSlideRays(_, rookDeltas))
  val queenMoveLookup = coords.map(genSlideRays(_, bishopDeltas ++ rookDeltas))

  def generateSlidingRay(board: Board, ray: Seq[Move], forSide: Int): Seq[Move] = {
    val takeIndex = ray.indexWhere((m: Move) => board(m.to) != empty)
    if (takeIndex == -1) ray
    else {
      val pa = board(ray(takeIndex).to)
      val captureExtend: Int = if (pa.side != forSide) 1 else 0
      ray.slice(0, takeIndex + captureExtend)
    }
  }

  class PawnEnPesant(from: Int, to: Int) extends Move(from, to) {
    val lift = to + (if (from < to) -8 else 8)

    override def play(b: Board) = {
      super.play(b)
      b(lift) = empty
    }

    override def toString() = {
      super.toString() + "ep=" + squareToString(lift)
    }
  }

  class PawnDoubleMove(from: Int, to: Int) extends Move(from, to) {
    val enPesantTarget = (from + to) / 2

    override def play(b: Board) = {
      super.play(b)
      b.ep_target = enPesantTarget
    }
  }

  class PromotionMove(from: Int, to: Int, promotesTo: Piece) extends Move(from, to) {
    //assert(promotesTo!=null)
    override def play(board: Board): Unit = {
      super.play(board)
      board(to) = promotesTo
    }

    def promoter = promotesTo

    override def toString() = {
      super.toString() + "=" + promotesTo.glyph
    }
  }

  def pawnMoveToPromoters(m: Move): List[Move] = {
    if (m.to < 8) List(wKnight, wBishop, wRook, wQueen).map(new PromotionMove(m.from, m.to, _))
    else if (m.to >= a1) List(bKnight, bBishop, bRook, bQueen).map(new PromotionMove(m.from, m.to, _))
    else List(m)
  }

  def pawnSingle(sq: Int, side: Int) = pawnMoveToPromoters(new Move(sq, sq - side * 8))

  def pawnDouble(sq: Int, side: Int): List[PawnDoubleMove] = {
    if (getRank(sq) == 1 && side == -1) List(new PawnDoubleMove(sq, sq + 16))
    else if (getRank(sq) == 6 && side == 1) List(new PawnDoubleMove(sq, sq - 16))
    else List()
  }

  def pawnCapture(sq: Int, side: Int): List[Move] = {
    val leftCapts = if (getFile(sq) != 0) pawnMoveToPromoters(new Move(sq, sq - 1 - side * 8)) else List()
    val rightCapts = if (getFile(sq) != 7) pawnMoveToPromoters(new Move(sq, sq + 1 - side * 8)) else List()
    (leftCapts ++ rightCapts).filter(m => m.to >= 0 && m.to <= 63)
  }

  def pawnEnPesant(sq: Int, side: Int): List[PawnEnPesant] = {
    if (side == 1 && getRank(sq) != 3) return List()
    if (side == -1 && getRank(sq) != 4) return List()
    //val c = C.fromSquare(sq)
    val leftEP = if (getFile(sq) != 0) Some(new PawnEnPesant(sq, sq - side * 8 - 1)) else None
    val rightEP = if (getFile(sq) != 7) Some(new PawnEnPesant(sq, sq - side * 8 + 1)) else None
    List(leftEP, rightEP).flatMap(f => f)
  }

  case class PawnPackage(singles: List[Move], doubles: List[PawnDoubleMove], captures: List[Move], epMoves: List[PawnEnPesant]) {

  }

  //val emptyPawnPackage = PawnPackage(List(), List(), List(), List())

  def pawnMoves(side: Int)(sq: Int) = PawnPackage(pawnSingle(sq, side), pawnDouble(sq, side), pawnCapture(sq, side), pawnEnPesant(sq, side))

  def pawnTable(side: Int) = squares.map(pawnMoves(side) _)

  //the following 'lazy' is a band-aid to solve the issue that pawn promotion move precomputation
  //runs before the piece classes are initialized
  lazy val whitePawnTable = pawnTable(1)
  lazy val blackPawnTable = pawnTable(-1)

  def pawnGen(table: IndexedSeq[PawnPackage])(b: Board, sq: Int, toMove: Int) = {
    val pp = table(sq)
    pp.singles.filter(m => b(m.to) == empty) ++
      pp.doubles.filter(m => (b(m.enPesantTarget) == empty) && (b(m.to) == empty)) ++
      pp.captures.filter(m => b(m.to).side == -toMove) ++
      pp.epMoves.filter(m => m.to == b.ep_target)
  }

  val whitePawnGen = pawnGen(whitePawnTable) _
  val blackPawnGen = pawnGen(blackPawnTable) _


  def sliderMoveGen(lookup: IndexedSeq[Seq[IndexedSeq[Move]]])(board: Board, square: Int, forSide: Int) =
    lookup(square).flatMap(generateSlidingRay(board, _, forSide))

  val bishopMoveGen = sliderMoveGen(bishopMoveLookup) _
  val rookMoveGen = sliderMoveGen(rookMoveLookup) _
  val queenMoveGen = sliderMoveGen(queenMoveLookup) _

  val centralize = Array(0, 3, 5, 10, 10, 5, 3, 0,
    3, 5, 10, 15, 15, 10, 5, 3,
    10, 15, 25, 25, 25, 25, 15, 10,
    10, 20, 35, 45, 45, 35, 20, 10,
    10, 20, 35, 45, 45, 35, 20, 10,
    10, 15, 25, 25, 25, 25, 15, 10,
    3, 5, 10, 15, 15, 10, 5, 3,
    0, 3, 5, 10, 10, 5, 3, 0)

  val blackPassedPawnBonuses = squares.map(sq => if (sq <= h8 || sq >= a1) 0 else (2 << (getRank(sq) + 2))).toArray
  val whitePassedPawnBonuses = blackPassedPawnBonuses.reverse

  val negCentralize = centralize.map(sq => -sq)
  val flat = centralize.map(sq => 0)

  //basic piece definitions
  case class Piece(glyph: String, side: Int, value: Int, movegen: (Board, Int, Int) => Seq[Move],
                   simpleEval: Array[Int], slowEval: (Int, Board, PawnEval) => Int) {
    val hashes = squares.map(x => if (value == 0) 0L else r.nextLong())
  }

  val RookSeventhBonus = 50

  def rookSeventh(rank: Int) = squares.map(getRank).map(squareRank => if (squareRank == rank) RookSeventhBonus else 0).toArray

  //TODO: fill these in and we're about done
  def dummySlowEval(sq: Int, b: Board, pe: PawnEval) = 0 // TODO

  def scoreFile(state: Int) = state match {
    case `closed` => 0
    case `open` => 60
    case `semiOpen` => 30
  }

  def scoreWhiteRook(sq: Int, b: Board, pe: PawnEval) = scoreFile(pe.whiteFileState(getFile(sq)))

  def kingSafety(sq: Int, otherMaterial: Int, pe: PawnEval) = {
    val armyStrength = otherMaterial / fullArmyValue
    println("Army strength = " + armyStrength)
    val neg = negCentralize(sq) * armyStrength * 7
    val pos = centralize(sq) * (1.0 - armyStrength) * 7
    println(s"neg:$neg, pos:$pos")
    val kSafety = neg + pos

    kSafety.toInt
  }

  def whiteKingSafety(sq: Int, b: Board, pe: PawnEval) = {
    kingSafety(sq, b.blackNonPawnMaterial, pe)
  }

  //def whitePassedPawnBonus(sq: Int, pe: PawnEval) = if (pe.whitePawnPassedAt(sq)) whitePassedPawnBonuses(sq) else 0
  //def whiteIsolatedPenalty(file: Int, pe: PawnEval) = if (pe.whitePawnIsolatedOn(file)) pe.IsolatedPawnPenalty else 0


  //def whiteDoubledPenalty(sq: Int, pe: PawnEval) = 0

  def whitePawnSlowEval(sq: Int, b: Board, pe: PawnEval) = {
    pe.whitePawnPassedAt(sq) +
      pe.whitePawnIsolatedOn(getFile(sq)) +
      pe.whitePawnDoubledOn(getFile(sq))
  }

  //def blackPassedPawnBonus(sq: Int, pe: PawnEval) = if (pe.blackPawnPassedAt(sq)) blackPassedPawnBonuses(sq) else 0

  def blackPawnSlowEval(sq: Int, b: Board, pe: PawnEval) = {
    pe.blackPawnPassedAt(sq) +
      pe.blackPawnIsolatedOn(getFile(sq)) +
      pe.blackPawnDoubledOn(getFile(sq))
  }

  def scoreBlackRook(sq: Int, b: Board, pe: PawnEval) = scoreFile(pe.blackFileState(getFile(sq)))

  def blackKingSafety(sq: Int, b: Board, pe: PawnEval) = {
    -kingSafety(sq, b.whiteNonPawnMaterial, pe)
  }

  def whiteKnightSlowEval(sq: Int, b: Board, pe: PawnEval) = 0 //TODO: king tropism?

  //distance to king
  def blackKnightSlowEval(sq: Int, b: Board, pe: PawnEval) = 0 //distance to king

  val empty = Piece(" ", 0, 0, knightMoveGen, flat, dummySlowEval)

  val wPawn = Piece("P", 1, 100, whitePawnGen, centralize, whitePawnSlowEval)
  val wKnight = Piece("N", 1, 325, knightMoveGen, centralize, whiteKnightSlowEval)
  val wBishop = Piece("B", 1, 350, bishopMoveGen, centralize, dummySlowEval)
  val wRook = Piece("R", 1, 500, rookMoveGen, rookSeventh(1), scoreWhiteRook)
  val wQueen = Piece("Q", 1, 900, queenMoveGen, flat, dummySlowEval)
  val wKing = Piece("K", 1, 10000, kingMoveGen, flat, whiteKingSafety)

  val bPawn = Piece("p", -1, -100, blackPawnGen, negCentralize, blackPawnSlowEval)
  val bKnight = Piece("n", -1, -325, knightMoveGen, negCentralize, blackKnightSlowEval)
  val bBishop = Piece("b", -1, -350, bishopMoveGen, negCentralize, dummySlowEval)
  val bRook = Piece("r", -1, -500, rookMoveGen, rookSeventh(6), scoreBlackRook)
  val bQueen = Piece("q", -1, -900, queenMoveGen, flat, dummySlowEval)
  val bKing = Piece("k", -1, -10000, kingMoveGen, centralize, blackKingSafety)

  val fullArmyValue = (wQueen.value + 2 * (wRook.value + wBishop.value + wKnight.value)).toFloat

  val pieces = List(
    empty,
    wPawn, wKnight, wBishop, wRook, wQueen, wKing,
    bPawn, bKnight, bBishop, bRook, bQueen, bKing)

  def moveGen(b: Board) = {
    squares.flatMap(sq => if (b(sq).side == b.toMove) b(sq).movegen(b, sq, b.toMove) else List())
  }

  def charToPiece(pStr: String): Option[Piece] = pieces.find(p => p.glyph == pStr)

  def boardToString(squares: IndexedSeq[String]) = {
    def sq(rank: Int = 0, file: Int = 0) = squares(rank * 8 + file)
    val sb = new StringBuilder()
    val part = "+---" * 8 + "+\n"
    for (rank <- 0 to 7) {
      sb.append(part)
      for (file <- 0 to 7) {
        sb.append("| " + sq(rank = rank, file = file) + " ")
      }
      sb.append("|\n")
    }
    sb.append(part)
    sb.toString
  }


  case class Board(squares: Array[Piece],
                   toMove: Int,
                   var castlingRight: Set[Char],
                   var ep_target: Int,
                   var material: Int, var whiteMaterial: Int, var blackMaterial: Int,
                   var hash: Long, var pawnHash: Long,
                   var whiteKingAt: Int, var blackKingAt: Int, var lastCaptureAt: Int,
                   var simpleEval: Int,
                   var whitePawnMap: Long, var blackPawnMap: Long,
                   var whitePawnCount: Int, var blackPawnCount: Int) {

    def apply(square: Int): Piece = squares(square)

    def whiteNonPawnMaterial = whiteMaterial - 100 * whitePawnCount

    def blackNonPawnMaterial = blackMaterial - 100 * blackPawnCount

    def update(square: Int, p: Piece) = {
      val removing = squares(square)
      if (removing != empty) {
        //removing piece
        if (removing.value < 0) blackMaterial += removing.value
        if (removing.value > 0) whiteMaterial -= removing.value

        if (removing == wPawn) {
          whitePawnCount = whitePawnCount - 1
          whitePawnMap = whitePawnMap & ~bitPawn(square)
        }

        if (removing == bPawn) {
          blackPawnCount = blackPawnCount - 1
          blackPawnMap = blackPawnMap & ~bitPawn(square)
        }

        material -= removing.value
        simpleEval -= (removing.value + removing.simpleEval(square))
        hash ^= removing.hashes(square)
        if (Math.abs(removing.value) == 100) pawnHash ^= removing.hashes(square)
      }
      squares(square) = p
      if (p.value < 0) blackMaterial -= p.value
      if (p.value > 0) whiteMaterial += p.value
      if (p == wKing) whiteKingAt = square
      if (p == bKing) blackKingAt = square
      material += p.value
      simpleEval += p.value + p.simpleEval(square)
      hash ^= p.hashes(square)
      if (Math.abs(p.value) == 100) pawnHash ^= p.hashes(square)
      if (p == wPawn) {
        whitePawnCount = whitePawnCount + 1
        whitePawnMap = whitePawnMap | bitPawn(square)
      }

      if (p == bPawn) {
        blackPawnCount = blackPawnCount + 1
        blackPawnMap = blackPawnMap | bitPawn(square)
      }
      if (square == e1) castlingRight = castlingRight - 'K' - 'Q'
      if (square == e8) castlingRight = castlingRight - 'k' - 'q'
      if (square == h1) castlingRight = castlingRight - 'K'
      if (square == a1) castlingRight = castlingRight - 'Q'
      if (square == h8) castlingRight = castlingRight - 'k'
      if (square == a8) castlingRight = castlingRight - 'q'
    }

    override def toString(): String = {
      boardToString(squares.map(_.glyph)) + "\n" + toMove
    }

    def makeChild() = {
      Board(squares.clone, -toMove, castlingRight, -1, material, whiteMaterial, blackMaterial,
        hash, pawnHash,
        whiteKingAt, blackKingAt, -1, simpleEval,
        whitePawnMap, blackPawnMap, whitePawnCount, blackPawnCount)
    }

    override def hashCode(): Int = hash.toInt

    override def equals(o: Any): Boolean = o match {
      case b2: Board => (hash == b2.hash) && (toMove == b2.toMove) // && (squares == b2.squares)
      case _ => false
    }
  }

  def startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def fromFEN(fenStr: String): Board = {
    val Array(pieces, toMoveTok, castlingRights, ep_square_str, halfmove_clock, fullmoves) = fenStr.split(' ')
    def charToPieces(ch: Char): List[Piece] = {
      val pm = charToPiece("" + ch)
      pm match {
        case Some(pc) => return List(pc)
        case None => Unit
      }
      if (ch > '0' && ch < '9') {
        return List.fill(ch - '0')(empty)
      }
      List()
    }
    val pieceSquares = pieces.flatMap(charToPieces)

    val toMove = toMoveTok match {
      case "w" => 1
      case "b" => -1
    }
    val ep_square: Int = Try(ep_square_str.toInt) getOrElse -1
    val board = Board(pieceSquares.map(_ => empty).toArray, toMove, Set(), ep_square, 0, 0, 0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0)
    pieceSquares.zipWithIndex.map({ case (p, sq) => if (p != empty) board(sq) = p })
    board.castlingRight = castlingRights.toSet
    board
  }

  val startBoard = fromFEN(startFEN)

  val boardHash = scala.collection.mutable.WeakHashMap[Board, Board]()

  def play(board: Board, move: Move): Board = {
    val newBoard = board.makeChild()
    if (board(move.to) != empty)
      newBoard.lastCaptureAt = move.to
    move.play(newBoard)
    if (boardHash.contains(newBoard) == true) {
      return boardHash.get(newBoard).get
    }
    boardHash.put(newBoard, newBoard)
    newBoard
  }

  def getEnteredMove(board: Board) = {
    val moves = moveGen(board)
    val line = readLine()
    if (line == "q")
      throw new Exception()
    val m = moves.filter(_.toString() == line)
    if (m.length == 0) {
      println(line + "--no matching move")
      None
    }
    else if (m.length > 1) {
      printf(line + "--move is ambiguous")
      None
    }
    else {
      Some(m(0))
    }
  }


  def bitPawn(square: Int) = 1L << square

  val pawnColumn = (a7 to a2 by 8).map(bitPawn).reduce(_ | _)

  def pawnTrace(square: Int, direction: Int): Long = {
    val nextSquare = square + direction * 8
    if (nextSquare > h8 && nextSquare < a1) (bitPawn(nextSquare) | pawnTrace(nextSquare, direction)) else 0L
  }

  def pawnMask(direction: Int)(sq: Int) = {
    pawnTrace(sq, direction) |
      (if (getFile(sq) != 0) pawnTrace(sq - 1, direction) else 0L) |
      (if (getFile(sq) != 7) pawnTrace(sq + 1, direction) else 0L)
  }

  val whitePassedPawnMasks = squares.map(pawnMask(-1) _)
  val blackPassedPawnMasks = squares.map(pawnMask(1) _)

  val (closed, semiOpen, open) = (1, 2, 3)

  case class PawnEval(val board: Board) {
    //The following vals need to be computed once
    val IsolatedPawnPenalty = -35
    val DoubledPawnPenalty = -35


    var timesUsed = 0

    def reset = timesUsed = 0

    def inc = timesUsed = timesUsed + 1

    def pawnMask(file: Int) = pawnColumn << file

    private def pawnsInColumn(map: Long, col: Int) = map & pawnMask(col)

    def whitePawnsInColumn(col: Int) = pawnsInColumn(board.whitePawnMap, col)

    def blackPawnsInColumn(col: Int) = pawnsInColumn(board.blackPawnMap, col)

    def fileState(file: Int, myMap: Long, theirMap: Long) = {
      if (pawnsInColumn(myMap, file) != 0) closed
      else if (pawnsInColumn(theirMap, file) != 0) semiOpen
      else open
    }

    def whiteFileState(file: Int) = fileState(file, board.whitePawnMap, board.blackPawnMap)

    def blackFileState(file: Int) = fileState(file, board.blackPawnMap, board.whitePawnMap)

    def whitePawnPassedAt(sq: Int) = if ((whitePassedPawnMasks(sq) & board.blackPawnMap) == 0L) whitePassedPawnBonuses(sq) else 0

    def blackPawnPassedAt(sq: Int) = if ((blackPassedPawnMasks(sq) & board.whitePawnMap) == 0L) blackPassedPawnBonuses(sq) else 0

    def countRowBits(bitBoard: Long) = {
      var bb = bitBoard
      var sum = 0
      while (bb > 0) {
        if ((bb & 0xff) != 0)
          sum = sum + 1
        bb = bb >> 8
      }
      sum
    }

    def isolatedPawnPunch(file: Int): Long = {
      (if (file > 0) pawnMask(file - 1) else 0) |
        (if (file < 7) pawnMask(file + 1) else 0)
    }

    def whitePawnIsolatedOn(file: Int) = if ((isolatedPawnPunch(file) & board.whitePawnMap) == 0) IsolatedPawnPenalty else 0

    def whitePawnDoubledOn(file: Int) = if (countRowBits(board.whitePawnMap & pawnMask(file)) > 1) DoubledPawnPenalty else 0

    def blackPawnIsolatedOn(file: Int) = if ((isolatedPawnPunch(file) & board.blackPawnMap) == 0) IsolatedPawnPenalty else 0

    def blackPawnDoubledOn(file: Int) = if (countRowBits(board.blackPawnMap & pawnMask(file)) > 1) DoubledPawnPenalty else 0
  }

  val pawnEvalHash = scala.collection.mutable.HashMap[Long, PawnEval]()

  def ageAndClearPawnHash = {
    val keysToDrop = pawnEvalHash.toSeq.filter(tup => tup._2.timesUsed == 0).map(tup => tup._1)
    println(s"${keysToDrop.size} keys aged out of the pawn hash")
    for (key <- keysToDrop) {
      pawnEvalHash.remove(key)
    }
    pawnEvalHash.foreach(tup => tup._2.reset)
  }

  def slowBoardEval(board: Board): Int = {
    if (pawnEvalHash.contains(board.pawnHash) == false) {
      pawnEvalHash += (board.pawnHash -> PawnEval(board))
    }
    val ph = pawnEvalHash.get(board.pawnHash).get
    ph.inc

    squares.map(sq => board(sq).slowEval(sq, board, ph)).sum
  }

  case class SearchTreeNode(board: Board) {
    lazy val boxes = moveGen(board).map(m => Box(board, m)).toBuffer
    private val fastEval = board.simpleEval * board.toMove
    private lazy val slowEval = slowBoardEval(board) * board.toMove

    def eval = fastEval + slowEval
  }

  //to do: use weak references, keep the tree more in RAM between collections
  case class Box(board: Board, move: Move) {
    lazy val child = SearchTreeNode(play(board, move))
  }

  object Stats {
    var nodes: Int = 0
    var quiesces: Int = 0
    var start: Long = 0

    def init = {
      start = System.currentTimeMillis()
      nodes = 0
      quiesces = 0
    }

    def report() = {
      val end = System.currentTimeMillis()
      val seconds = (end.toDouble - start.toDouble) / 1000.0;
      println(s"searched $nodes nodes with $quiesces quiesces in $seconds seconds")
    }
  }

  def negamax(node: SearchTreeNode, depth: Int, _alpha: Int, beta: Int): (Int, List[Move]) = {
    var alpha = _alpha
    Stats.nodes = Stats.nodes + 1


    if (depth <= 0 && node.board.lastCaptureAt == -1) {
      return (node.eval, Nil)
    }

    var bestValue = if (depth > 0) Int.MinValue else node.eval
    var bestLine: List[Move] = Nil

    var children = node.boxes

    if (depth <= 0) Stats.quiesces = Stats.quiesces + 1

    var moveNum: Int = 0
    while (alpha < beta && moveNum < children.length) {
      val child = children(moveNum)
      if (canCaptureKing(child.board)) {
        children.remove(moveNum) // remove moves that allow me to be mated
      }
      else {
        if (depth > 0 || child.move.to == node.board.lastCaptureAt) {
          val (childVal, _childLine) = negamax(child.child, depth - 1, -beta, -alpha)
          val nodeScore = -childVal
          if (nodeScore > bestValue) {
            bestValue = nodeScore
            bestLine = child.move :: _childLine
            for (i <- moveNum to 1 by -1) {
              val c = children(i); children(i) = children(i - 1); children(i - 1) = c
            }
          }
          alpha = Math.max(alpha, nodeScore)
        }
        moveNum = moveNum + 1
      }
    }

    /*if (bestValue < -999000) bestValue = bestValue + 1
    if (bestValue > 999000) bestValue = bestValue - 1

    if (children.length == 0) {
      bestValue = if (kingToMoveInCheck(node.board) == true) -999999 else 0
    }*/

    //clean some tree
    /*moveNum = Math.min(moveNum, 5)
    while (moveNum < children.length) {
      children(moveNum).clear
      moveNum = moveNum + 1
    }*/

    return (bestValue, bestLine)
  }


  def search(curr: Board, maxDepth: Int) = {
    val tree = SearchTreeNode(curr)
    Stats.start
    var score = 0
    var line: List[Move] = Nil
    for (i <- 1 to maxDepth) {
      var low = -20000
      var high = 20000
      var exact: Option[Int] = None
      while (exact == None) {
        val guess = (low + high) / 2
        println(s"starting dept $i search with low: $low high:$high guess:$guess")
        val (wl, wh) = (guess - 2, guess + 2)

        val (_score, _line) = negamax(tree, i, wl, wh)
        score = _score
        line = _line

        if (score <= wl) high = guess
        else if (score >= wh) low = guess
        else exact = Some(score)
      }
      println("Computer chose " + line + " with a score of " + score)
    }
    Stats.report()

    (curr, line(0))
  }

  def main() = {
    var curr = startBoard
    while (true) {
      println(curr)
      ageAndClearPawnHash
      println("Pawn Hash size:" + pawnEvalHash.size)
      println("Hash size:" + boardHash.size)
      boardHash.empty
      println(moveGen(curr))
      System.gc()
      val mv = getEnteredMove(curr)
      mv match {
        case Some(m) =>
          curr = play(curr, m)
          println(curr)
          val (score, move) = search(curr, 4)
          curr = play(curr, move)

        case None =>

      }
    }
  }
}