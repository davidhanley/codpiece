
import Data.Array
import Data.List

data Move = Move { from   :: Int ,
                   to     :: Int , 
                   player :: (Array Piece Int) -> (Array Piece Int) } -- deriving( Eq, Show ) 

data Piece = Piece { side  :: Int , 
                     value :: Int ,
                     glyph :: Char , 
                     generator :: (Array Int Piece) -> Int -> [Move] } -- deriving(Eq)

files = [ 'a'..'h' ]
ranks = reverse [ '1'..'8' ]
squares = [0..63]

square_to_string sq = [ files !! r , ranks !! f ] where (r,f) = (square_to_coord sq) 

instance Show Move where
  show m = (square_to_string (from m)) ++ "-" ++ (square_to_string (to m))

instance Eq Move where
  (==) m1 m2 = (from m1)==(from m2) && (to m1)==(to m2)

instance Show Piece where
  show p = [glyph p] 

instance Eq Piece where
  (==) p1 p2 = (glyph p1)==(glyph p2) 

wPiece = Piece 1
bPiece = Piece (-1)

dummy_generator::  Array Int Piece -> Int -> [Move]
dummy_generator a i = []

empty   = Piece 0 0 ' ' dummy_generator

axis_ok x               = x >= 0 && x < 8
coord_ok ( r , f )      = axis_ok r && axis_ok f
square_to_coord :: Int -> ( Int , Int )
square_to_coord sq      = ( mod sq 8 , div sq 8  ) 
coord_to_square (f,r)   = r * 8 + f 
add_coord (f,r) (f2,r2) = (f+f2,r+r2)


--
-- Move generation. Pieces have functions that take a board and a 'from' square, then return a list of pseudolegal
-- moves. 
simple_move f t = Move f (coord_to_square t) (\x->x)

knight_deltas :: [(Int, Int)]
knight_deltas = [ (x,y) | x<-[-2..2] , y<-[-2..2] , (abs x) /= (abs y) , x/=0, y/=0 ] 
king_deltas :: [(Int, Int)]
king_deltas   = [ (x,y) | x<-[-1..1] , y<-[-1..1] , not ( x==0 && y==0 ) ]  

--given a square and a list of deltas, make a list of moves a piece on that square could potnetially move to
generate_hopper deltas sq = 
  map (simple_move sq) $ filter coord_ok $ map (add_coord (square_to_coord sq)) deltas

--given a s
generate_ray sq delta = map (simple_move sq)  $ tail $ takeWhile coord_ok $ iterate (\s->add_coord s delta) (square_to_coord sq) 
generate_slider deltas sq =  map (\delta->generate_ray sq delta) deltas

--
-- Pawns
--
pawn_move side sq = 
  let  (file,rank)=square_to_coord sq
       pawn_to = simple_move sq 
       double_jumper double_rank double_side =  if rank /= double_rank || side /= double_side then [] else [pawn_to (file,rank-side*2) ]
       pawncapt capfile direc = if file/=capfile then [pawn_to (file+direc,rank-side)] else []
  in 
  if (rank==0) || (rank==7) then ([],[]) else (
    [ pawn_to (file,rank-side) ] ++ ( double_jumper 6 1 ) ++ ( double_jumper 1 (-1) ) ,
    ( pawncapt 0 (-1) ) ++ ( pawncapt 7 1 ) ) 

white_pawn_table = make_lookup_table $ pawn_move 1
black_pawn_table = make_lookup_table $ pawn_move (-1)

pawn_move_generator table myside board sq =
  let (moves,capts) = table ! sq in 
  (takeWhile (\move->(board ! (to move))==empty)            moves) ++
  (filter    (\move->side (board ! (to move)) == (-myside)) capts) 
  

--the rays along which bishops and rooks can move 
(rook_deltas,bishop_deltas) = partition (\(x,y)->x==0||y==0) king_deltas 

make_lookup_table cgf = array (0,63) $ zip squares (map cgf squares)

hop_table    =  make_lookup_table . generate_hopper 
knight_table = hop_table knight_deltas
king_table   = hop_table king_deltas

hopper_move_generator ::  Array Int [Move] -> Int -> Array Int Piece -> Int -> [Move]
hopper_move_generator table my_side board square =
  filter (\move->(side (board ! (to move))) /= my_side) (table ! square)

slider_table_gen = make_lookup_table . generate_slider
bishop_table = slider_table_gen bishop_deltas
rook_table   = slider_table_gen rook_deltas
queen_table  = slider_table_gen king_deltas

slider_move_generator table my_side board square = 
  concat $ map trace_ray $ table ! square where 
     trace_ray [] = [] 
     trace_ray (move:rest) = 
       let target_side = (side (board ! (to move))) in
         if ( target_side == my_side ) then [] else (
           if target_side == (-my_side) then [move] else (move:(trace_ray rest)))

wPawn   = wPiece   100 'p' $ pawn_move_generator white_pawn_table 1 
wKnight = wPiece   325 'n' $ hopper_move_generator knight_table 1
wBishop = wPiece   350 'b' $ slider_move_generator bishop_table 1
wRook   = wPiece   500 'r' $ slider_move_generator rook_table 1
wQueen  = wPiece   900 'q' $ slider_move_generator queen_table 1
wKing   = wPiece 10000 'k' $ hopper_move_generator king_table 1

bPawn   = bPiece 100 'P' $ pawn_move_generator black_pawn_table (-1)
bKnight = bPiece 325 'N' $ hopper_move_generator knight_table (-1)
bBishop = bPiece 350 'B' $ slider_move_generator bishop_table (-1)
bRook   = bPiece 500 'R' $ slider_move_generator rook_table (-1)
bQueen  = bPiece 900 'Q' $ slider_move_generator queen_table (-1)
bKing   = bPiece 10000 'K' $ hopper_move_generator king_table (-1)

pieces = [ empty , 
           wPawn , wKnight , wBishop , wRook , wQueen , wKing , 
           bPawn , bKnight , bBishop , bRook , bQueen , bKing ]

char2piece pc = ictp pc pieces where
  ictp pc [] = empty
  ictp pc (f:r) = if glyph f==pc then f else (ictp pc r)   


cahr2piece pc = case (find (\piece->pc == glyph piece) pieces) of
                  Just pc->pc
                  Nothing->empty

startBoard = "RNBQKBNR" ++
             "PPPPPPPP" ++
             (concat (replicate 4 "        ")) ++
             "ppp pppp" ++
             "rnbqkbnr"

  
board :: (Array Int Piece)
board = array (0,63) $ zip [0..63] (map char2piece startBoard)

movegen board for_side = 
  concat $ map (\(sq,pc)->if for_side==(side pc) then (generator pc)  board sq else [] ) (assocs board)
  
print_board b =
  unlines $ [print_horiz]++(concat (map print_rank [0..7])) where
    print_rank r = [(concat (map (\f->"| "++[glyph (board ! (r*8+f))] ++ " ") [0..7])) ++ "|",print_horiz]
    print_horiz = "+" ++ ( concat (replicate 8 "---+"))

--board_material = foldl (\cv pc->cv + (value pc)*(side pc)) 

pb _ = putStrLn $ print_board board 

 
main = print "chesssss"
