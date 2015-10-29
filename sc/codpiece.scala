import scala.util._

object Codpiece {

  val squares = 0 to 63

  def rankToChar(r: Int) = "87654321"(r)

  def fileToChar(f: Int) = "abcdefgh"(f)

  def squareToString(sq: Int) = "" + fileToChar(sq % 8) + rankToChar(sq / 8)

  val r = scala.util.Random

  case class Coord(r:Int,f:Int) {
    def Coord(sq:Int) = new Coord(sq/8,sq%8)
    def toSquare = r * 8 + f
  }


  //basic piece definitions
  case class Piece(glyph: String, side: Int, value: Int) {
    val hashes = squares.map( x => r.nextLong() )
  }


  /*class hoppingPiece(glyph: String, side: Int, value: Int) extends Piece(glyph, side, value) {
    def generateMoves( board: Board ):Seq[Move] = {

    }
  }*/

  val empty = Piece(" ", 0, 0)

  val wPawn = Piece("P", 1, 100)
  val wKnight = Piece("N", 1, 325)
  val wBishop = Piece("B", 1, 350)
  val wRook = Piece("R", 1, 500)
  val wQueen = Piece("Q", 1, 900)
  val wKing = Piece("K", 1, 10000)

  val bPawn = Piece("p", 1, -100)
  val bKnight = Piece("n", 1, -325)
  val bBishop = Piece("b", 1, -350)
  val bRook = Piece("r", 1, -500)
  val bQueen = Piece("q", 1, -900)
  val bKing = Piece("k", 1, -10000)

  val pieces = List(
    empty,
    wPawn, wKnight, wBishop, wRook, wQueen, wKing,
    bPawn, bKnight, bBishop, bRook, bQueen, bKing)

  def charToPiece(pStr: String): Option[Piece] = pieces.find(p => p.glyph == pStr)


  case class Board(squares: Array[Piece],
                   toMove: Int,
                   var castlingRight: Set[Char],
                   var ep_target: Int,
                   var material: Int, var whiteMaterial:Int, var blackMaterial:Int,
                   var hash: Long, var pawnHash:Long ) {
    def sq(rank: Int = 0, file: Int = 0) = squares(rank * 8 + file)

    def apply(square:Int):Piece = squares(square)

    def update(square:Int,p:Piece) = {

      val removing = squares(square)
      if (removing!=empty) { //removing piece

        material -= removing.value
        if (removing.value<0) blackMaterial += removing.value
        if (removing.value>0) whiteMaterial -= removing.value
        material -= removing.value
        hash ^= removing.hashes(square)
        if (removing.value==100) pawnHash ^= removing.hashes(square)
      }
      squares(square)=p
      if (p.value<0) blackMaterial -= p.value
      if (p.value>0) whiteMaterial += p.value
      material += p.value
      hash ^= p.hashes(square)
      if (p.value==100) pawnHash ^= p.hashes(square)
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
      Board(squares.clone,-toMove,castlingRight,0,material,whiteMaterial,blackMaterial,hash,pawnHash)
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

    val ep_square:Int = Try(ep_square_str.toInt) match { case None=> -1; case Some(i:Int) => i }

    val board = Board(pieceSquares.map(_=>empty).toArray, toMove, castlingRights.toSet, ep_square,0,0,0,0,0)



    pieceSquares.zipWithIndex.map({case(p,sq)=>if (p!=empty) board(sq)=p})

    board
  }

  val startBoard = fromFEN(startFEN)

  def play(board: Board, move: Move) = {
    val newBoard = board.makeChild()
    move.play(newBoard)
    newBoard
  }

  def bench() = {
    val b = startBoard
    val st = System.currentTimeMillis()
    val m = new Move(48,48-16)
    var x = 0
    for( a <- 1 to 1000000) {
      //print(".")
      val b2 = play(b, m)
      x = x + b2.toMove
    }
    val et = System.currentTimeMillis()

    println(x)
    println("Time Taken:" + ( et-st) )
  }

  val (a8, e8, h8) = (0, 4, 7)
  val (a1, e1, h1) = (56, 60, 63)

  class Move(from: Int, to: Int) {
    def play(board: Board): Unit = {
      board(to) = board(from)
      board(from) = empty
      board.castlingRight = castlingImpact(board.castlingRight)
    }

    def castlingImpact(rights: Set[Char]): Set[Char] = {
      (from, to) match {
        case (`e8`, _) => rights - 'K' - 'Q'
        case (`e1`, _) => rights - 'k' - 'q'
        case (_, `a1`) => rights - 'Q'
        case (_, `h1`) => rights - 'K'
        case (_, `a8`) => rights - 'q'
        case (_, `h8`) => rights - 'k'
        case (_, _) => rights
      }
    }

    def toStrings():Seq[String] = {
      Seq(squareToString(from)+squareToString(to))
    }

    override def toString() = {
      toStrings()(0)
    }
  }

  class PromotionMove(from: Int, to: Int, promotesTo: Piece) extends Move(from, to) {

    override def play(board: Board): Unit = {
      super.play(board)
      board(to) = promotesTo
    }

    override def toStrings() = {
      toStrings.map( _ + "=" + promotesTo.glyph(0))
    }
  }



}