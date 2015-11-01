
import java.sql.SQLClientInfoException

import scala.collection.immutable
import scala.util._

object Codpiece {

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

  def getRank(sq:Int) = sq/8
  def getFile(sq:Int) = sq%8

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

  class CastlingMove(from:Int, to:Int, rookFrom:Int, rookTo:Int) extends Move(from, to) {
    override def play(b:Board): Unit = {
      super.play(b)
      b(rookTo)=b(rookFrom)
      b(rookFrom)=empty
    }
  }

  val whiteKingsideCastle = new CastlingMove(e1,g1,h1,f1)
  val whiteQueensideCastle = new CastlingMove(e1,c1,a1,d1)
  val blackKingsideCastle = new CastlingMove(e8,g8,h8,f8)
  val blackQueensideCastle = new CastlingMove(e8,c8,a8,d8)

  //TODO:fill this in
  def sideAttacks(sq:Int, side:Int)(implicit board:Board) = {
    false
  }

  def freeAndClear( side:Int, squares:Int* )( implicit board:Board ):Boolean = {
    //println(board)
    //println(squares)
    val r = squares.forall(sq=>board(sq) == empty) && squares.forall(sideAttacks(_, side) == false)
    //println(r)
    r
  }

  //TODO: this is kinda gross. Fix it
  def whiteCastle(implicit b:Board):Seq[Move]= {
    val wkc = if (b.castlingRight('K') && freeAndClear(1,f1,g1)) Some(whiteKingsideCastle) else None
    val wqc = if (b.castlingRight('Q') && freeAndClear(1,d1,c1) && b(b1)==empty ) Some(whiteQueensideCastle) else None
    List(wkc,wqc).flatMap(f=>f)
  }

  def blackCastle(implicit b:Board):Seq[Move]= {
    val bkc = if (b.castlingRight('k') && freeAndClear(-1,f8,g8)) Some(blackKingsideCastle) else None
    val bqc = if (b.castlingRight('q') && freeAndClear(-1,d8,c8) && b(b8)==empty ) Some(blackQueensideCastle) else None
    List(bkc,bqc).flatMap(f=>f)
  }

  def kingMoveGen(b: Board, sq: Int, forSide: Int) = {
    val basicKingMoves = hopperGen(kingMoveLookup)(b, sq, forSide)
    if (sq == e1 && forSide == 1) basicKingMoves ++ whiteCastle(b)
    else if (sq == e8 && forSide == -1) basicKingMoves ++ blackCastle(b)
    else basicKingMoves
  }

  def genSlideRay(source:C, delta:C) =
    (1 to 8).map(source+delta*_).filter(_.is_valid).map(coordsToMove(source,_))

  def genSlideRays(source:C, deltas:Seq[C]) = deltas.map(genSlideRay(source,_))

  val bishopDeltas = Array( C(-1,-1), C(-1,1), C(1,-1), C(1,1) )
  val rookDeltas = Array( C(-1,0), C(1,0), C(0,-1), C(0,1) )

  val bishopMoveLookup = coords.map( genSlideRays(_,bishopDeltas))
  val rookMoveLookup = coords.map( genSlideRays(_,rookDeltas))
  val queenMoveLookup = coords.map( genSlideRays(_,bishopDeltas++rookDeltas))

  def generateSlidingRay( board:Board, ray:Seq[Move], forSide:Int):Seq[Move] = {
    val takeIndex = ray.indexWhere((m: Move) => board(m.to) != empty)
    if ( takeIndex == -1 ) ray else {
      val pa = board(ray(takeIndex).to)
      val captureExtend: Int = if ( pa.side != forSide ) 1 else 0
      ray.slice(0, takeIndex + captureExtend)
    }
  }

  class PawnEnPesant(from:C, to:C) extends Move(from.toSquare,to.toSquare) {
    val lift = C(from.r,to.f).toSquare
    override def play( b:Board ) = {
      super.play(b)
      b(lift)=empty
    }
    override def toString() = { super.toString() + "ep"}
  }

  class PawnDoubleMove(from:Int,to:Int) extends Move(from,to) {
    val enPesantTarget = (from+to)/2
    override def play( b:Board ) = {
      super.play(b)
      b.ep_target = enPesantTarget
    }
    override def toString() = { super.toString() + "dpm"}
  }

  class PromotionMove(from: Int, to: Int, promotesTo: Piece) extends Move(from, to) {
    override def play(board: Board): Unit = {
      super.play(board)
      board(to) = promotesTo
    }
    override def toString() = { super.toString() + "=" + promotesTo.glyph }
  }

  val whitePrmotePieces = List( wKnight, wBishop, wRook, wQueen )
  val blackPrmotePieces = List( bKnight, bBishop, bRook, bQueen )

  def pawnMoveToPromoters(m:Move):List[Move] = {
    if (m.to < 8) whitePrmotePieces.map(new PromotionMove(m.from,m.to,_))
    else if (m.to >= a1) blackPrmotePieces.map(new PromotionMove(m.from,m.to,_))
    else List(m)
  }


  def pawnSingle(sq:Int, side:Int) = pawnMoveToPromoters(new Move(sq, sq-side*8))

  def pawnDouble(sq:Int, side:Int):List[PawnDoubleMove] = {
    if (getRank(sq)==1 && side == -1) List(new PawnDoubleMove(sq, sq+16))
    else if ( getRank(sq)==6 && side == 1 ) List(new PawnDoubleMove(sq, sq-16))
    else List()
  }
  def pawnCapture(sq:Int, side:Int):List[Move] = {
    val leftCapts = if (getFile(sq)!=0) pawnMoveToPromoters( new Move(sq,sq-1-side*8)) else List()
    val rightCapts = if (getFile(sq)!=7) pawnMoveToPromoters( new Move(sq,sq+1-side*8)) else List()
    leftCapts ++ rightCapts
  }

  def pawnEnPesant(sq:Int,side:Int):List[PawnEnPesant] = {
    if (side==1 && getRank(sq)!=3) return List()
    if (side== -1 && getRank(sq)!=4) return List()
    val c = C.fromSquare(sq)
    val leftEP = if (getFile(sq)!=0) Some( new PawnEnPesant(c,c+C(-side,-1))) else None
    val rightEP = if (getFile(sq)!=7) Some( new PawnEnPesant(c,c+C(-side,1))) else None
    List(leftEP,rightEP).flatMap(f=>f)
  }

  case class PawnPackage(singles:List[Move], doubles:List[PawnDoubleMove], captures:List[Move], epMoves:List[PawnEnPesant]) {

  }

  def pawnMoves(side:Int)(sq:Int)=PawnPackage(pawnSingle(sq, side),pawnDouble(sq, side),pawnCapture(sq, side),pawnEnPesant(sq,side))

  def pawnTable(side:Int) = squares.map(pawnMoves(side) _)

  val whitePawnTable = pawnTable(1)
  val blackPawnTable = pawnTable(-1)

  def pawnGen(table:IndexedSeq[PawnPackage])(b:Board, sq:Int, toMove:Int) = {
    val pp = table(sq)
    pp.singles.filter(m=>b(m.to)==empty) ++
    pp.doubles.filter(m=>b(m.enPesantTarget)==empty) ++
    pp.captures.filter(m=>b(m.to).side == -toMove) ++
    pp.epMoves.filter(m=>m.lift==b.ep_target)
  }

  val whitePawnGen = pawnGen(whitePawnTable) _
  val blackPawnGen = pawnGen(blackPawnTable) _





  def sliderMoveGen(lookup:IndexedSeq[Seq[IndexedSeq[Move]]])( board:Board, square:Int , forSide:Int) =
    lookup(square).flatMap(generateSlidingRay(board, _, forSide))

  val bishopMoveGen = sliderMoveGen(bishopMoveLookup) _
  val rookMoveGen = sliderMoveGen(rookMoveLookup) _
  val queenMoveGen = sliderMoveGen(queenMoveLookup) _

  //basic piece definitions
  case class Piece(glyph: String, side: Int, value: Int, movegen: (Board,Int,Int) => Seq[Move] )  {
    val hashes = squares.map( x => r.nextLong() )
  }

  val empty = Piece(" ", 0, 0, knightMoveGen)

  val wPawn = Piece("P", 1, 100, whitePawnGen)
  val wKnight = Piece("N", 1, 325, knightMoveGen)
  val wBishop = Piece("B", 1, 350, bishopMoveGen)
  val wRook = Piece("R", 1, 500, rookMoveGen)
  val wQueen = Piece("Q", 1, 900, queenMoveGen)
  val wKing = Piece("K", 1, 10000, kingMoveGen)

  val bPawn = Piece("p", -1, -100, blackPawnGen)
  val bKnight = Piece("n", -1, -325, knightMoveGen)
  val bBishop = Piece("b", -1, -350, bishopMoveGen)
  val bRook = Piece("r", -1, -500, rookMoveGen)
  val bQueen = Piece("q", -1, -900, queenMoveGen)
  val bKing = Piece("k", -1, -10000, kingMoveGen)

  val pieces = List(
    empty,
    wPawn, wKnight, wBishop, wRook, wQueen, wKing,
    bPawn, bKnight, bBishop, bRook, bQueen, bKing)

  def moveGen(b:Board) = {
    squares.flatMap(sq=>if (b(sq).side==b.toMove) b(sq).movegen(b,sq,b.toMove) else List() )
  }

  def charToPiece(pStr: String): Option[Piece] = pieces.find(p => p.glyph == pStr)


  case class Board(squares: Array[Piece],
                   toMove: Int,
                   var castlingRight: Set[Char],
                   var ep_target: Int,
                   var material: Int, var whiteMaterial:Int, var blackMaterial:Int,
                   var hash: Long, var pawnHash:Long ,
                   var whiteKingAt:Int, var blackKingAt:Int ) {
    def sq(rank: Int = 0, file: Int = 0) = squares(rank * 8 + file)

    def apply(square:Int):Piece = squares(square)

    def update(square:Int,p:Piece) = {

      val removing = squares(square)
      if (removing!=empty) { //removing piece
        if (removing.value<0) blackMaterial += removing.value
        if (removing.value>0) whiteMaterial -= removing.value
        material -= removing.value
        hash ^= removing.hashes(square)
        if (removing.value==100) pawnHash ^= removing.hashes(square)
      }
      squares(square)=p
      if (p.value < 0) blackMaterial -= p.value
      if (p.value > 0) whiteMaterial += p.value
      if (p.value == 10000) whiteKingAt = square
      if (p.value == -10000) blackKingAt = square
      material += p.value
      hash ^= p.hashes(square)
      if (p.value==100) pawnHash ^= p.hashes(square)
      if (square==e1) castlingRight = castlingRight - 'K' - 'Q'
      if (square==e8) castlingRight = castlingRight - 'k' - 'q'
      if (square==h1) castlingRight = castlingRight - 'K'
      if (square==a1) castlingRight = castlingRight - 'Q'
      if (square==h8) castlingRight = castlingRight - 'k'
      if (square==a8) castlingRight = castlingRight - 'q'
    }

    override def toString(): String = {
      val sb = new StringBuilder()
      val part = "+---" * 8 + "+\n"
      for (rank <- 0 to 7) {
        sb.append(part)
        for (file <- 0 to 7) {
          sb.append("| " + sq(rank = rank, file = file).glyph + " ")
        }
        sb.append("|\n")
      }
      sb.append(part)
      sb.toString
    }

    def makeChild() = {
      Board(squares.clone,-toMove,castlingRight,0,material,whiteMaterial,blackMaterial,hash,pawnHash,whiteKingAt,blackKingAt)
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
    val ep_square:Int = Try(ep_square_str.toInt) getOrElse -1
    val board = Board(pieceSquares.map(_=>empty).toArray, toMove, Set(), ep_square,0,0,0,0,0,-1,-1)
    pieceSquares.zipWithIndex.map({case(p,sq)=>if (p!=empty) board(sq)=p})
    board.castlingRight = castlingRights.toSet
    board
  }

  val startBoard = fromFEN(startFEN)

  def play(board: Board, move: Move) = {
    val newBoard = board.makeChild()
    move.play(newBoard)
    newBoard
  }









}