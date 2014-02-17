
-- Chess engine in haskell. Goal is to be strong expert/weak master ( ~2200 ) at blitz and < 1000 lines of simple non-obfuscated code 
-- By David Hanley ( ctchrinthy@gmail.com ) davidhanley.org
-- this sets up the pieces, plays legal moves, and can do efficient tree searches to choose the best move

import Data.Array
import Data.List
import Data.Maybe 

data Move = Move { from   :: Int ,
                   to     :: Int , 
                   player :: Board -> Board } 

data Piece = Piece { side  :: Int , 
                     value :: Int ,
                     glyph :: Char , 
                     generator :: Array Int Piece -> Int -> [Move] ,
                     evaluate  :: Array Int Piece -> Int -> Int }

data Board = Board { squares :: Array Int Piece , 
                     to_move :: Int ,
                     move_hist :: [(Piece,Int,Int)] }

simple_play::Int->Int->Board->Board
simple_play f t board = Board ( squares board // [ ( t , squares board ! f) , (f,empty) ] ) 
                               (-(to_move board)) 
                               (( squares board ! t , f , t ):move_hist board)

-- i don't need most of these, but it's kinda cool and easy 
[a8,b8,c8,d8,e8,f8,g8,h8,
 a7,b7,c7,d7,e7,f7,g7,h7,
 a6,b6,c6,d6,e6,f6,g6,h6,
 a5,b5,c5,d5,e5,f5,g5,h5,
 a4,b4,c4,d4,e4,f4,g4,h4,
 a3,b3,c3,d3,e3,f3,g3,h3,
 a2,b2,c2,d2,e2,f2,g2,h2,
 a1,b1,c1,d1,e1,f1,g1,h1]=[0..63]::[Int]

files = [ 'a'..'h' ]
ranks = reverse [ '1'..'8' ]
bsquares = [0..63]

(white,black) = (1,-1)

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

empty   = Piece 0 0 ' ' dummy_generator (\bd sq->0)

axis_ok x               = x >= 0 && x < 8
coord_ok ( r , f )      = axis_ok r && axis_ok f
square_to_coord :: Int -> ( Int , Int )
square_to_coord sq      = ( mod sq 8 , div sq 8  ) 
coord_to_square (f,r)   = r * 8 + f 
add_coord (f,r) (f2,r2) = (f+f2,r+r2)


--
-- Move generation. Pieces have functions that take a board and a 'from' square, then return a list of pseudolegal
-- moves.
--
 
simple_move f t = Move f (coord_to_square t) (simple_play f (coord_to_square t) )

knight_deltas :: [(Int, Int)]
knight_deltas = [ (x,y) | x<-[-2..2] , y<-[-2..2] , (abs x) /= (abs y) , x/=0, y/=0 ] 
king_deltas :: [(Int, Int)]
king_deltas   = [ (x,y) | x<-[-1..1] , y<-[-1..1] , not ( x==0 && y==0 ) ]  

--given a square and a list of deltas, make a list of moves a piece on that square could potnetially move to
generate_hopper deltas sq = 
  map (simple_move sq) $ filter coord_ok $ map (add_coord (square_to_coord sq)) deltas

generate_ray sq delta = map (simple_move sq)  $ tail $ takeWhile coord_ok $ iterate (\s->add_coord s delta) (square_to_coord sq) 
generate_slider deltas sq =  map (\delta->generate_ray sq delta) deltas

--
-- Pawns
--
pawn_move side sq = 
  let  (file,rank)=square_to_coord sq
       pawn_to (f,r) = [simple_move sq (f,r)]
       double_jumper double_rank double_side =  if rank /= double_rank || side /= double_side then [] else (pawn_to (file,rank-side*2) )
       pawncapt capfile direc = if file/=capfile then (pawn_to (file+direc,rank-side)) else []
  in 
  if (rank==0) || (rank==7) then ([],[]) else (
    ( pawn_to (file,rank-side) ) ++ ( double_jumper 6 1 ) ++ ( double_jumper 1 (-1) ) ,
    ( pawncapt 0 (-1) ) ++ ( pawncapt 7 1 ) ) 

white_pawn_table = make_lookup_table $ pawn_move 1
black_pawn_table = make_lookup_table $ pawn_move (-1)

pawn_move_generator table myside board sq =
  let (moves,capts) = table ! sq in 
  (takeWhile (\move->(board ! (to move))==empty)            moves) ++
  (filter    (\move->side (board ! (to move)) == (-myside)) capts) 

--the rays along which bishops and rooks can move 
(rook_deltas,bishop_deltas) = partition (\(x,y)->x==0||y==0) king_deltas 

make_lookup_table cgf = array (0,63) $ zip bsquares (map cgf bsquares)
make_lookup_table_rf func = make_lookup_table (func.square_to_coord)

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

-- see if a square is attacked by a side.  Used to determine if a side can castle 
-- i could also generate moves for a side and see if any of the "to" moves match the square,
-- but that seems horrid

hop_attacks board pc = any (\mv->board!(to mv)==pc) 

sliders_on_ray board _ []             = False
sliders_on_ray board pieces (move:rest)  = 
  let pc = board ! (to move) in
    if elem pc pieces then True else ( if pc == empty then sliders_on_ray board pieces rest else False )
    
attackbase board kp kt diag diagt hv hvt p pt king king_table square = 
  hop_attacks board kp (kt!square) || any (sliders_on_ray board diag ) (diagt!square)||
  any (sliders_on_ray board hv)(hvt!square)|| hop_attacks board p (snd (pt!square))|| hop_attacks board king (king_table!square)
attacks 1 board  = 
  attackbase board wKnight knight_table [wRook,wQueen] rook_table
   [wBishop,wQueen] bishop_table wPawn black_pawn_table wKing king_table
attacks (-1) board  = 
  attackbase board bKnight knight_table [bRook,bQueen] rook_table
   [bBishop,bQueen] bishop_table bPawn white_pawn_table bKing king_table

can_white_castle_kingside board = 
  let att sq = not $ attacks black board sq
      bp sq pc = board!sq == pc in
  if bp e1 wKing &&  bp f1 empty && bp g1 empty && bp h1 empty &&
     att e1 && att f1 && att g1 then True else False 

--
-- Positional evaluation.  At first, simple piece-square lookups. 
--

mp=[0,3,9,15,15,9,3,0]
center_tropism = make_lookup_table_rf (\(f,r)->(mp!!f)+(mp!!r)) 

to_center bs sq = center_tropism ! sq  
flee_center bs sq = -(center_tropism ! sq)

eval_sq sob (sq,pc) = (side pc)*
   ( ((evaluate pc) sob sq) +(value pc) )

eval board = let sob = (squares board) in sum ( map  (eval_sq sob) (assocs sob))

wPawn   = wPiece   100 'p' (pawn_move_generator white_pawn_table 1) to_center
wKnight = wPiece   325 'n' (hopper_move_generator knight_table 1) to_center
wBishop = wPiece   350 'b' (slider_move_generator bishop_table 1) to_center
wRook   = wPiece   500 'r' (slider_move_generator rook_table 1) to_center
wQueen  = wPiece   900 'q' (slider_move_generator queen_table 1) to_center
wKing   = wPiece 10000 'k' (hopper_move_generator king_table 1) flee_center

bPawn   = bPiece   100 'P' (pawn_move_generator black_pawn_table (-1))  to_center
bKnight = bPiece   325 'N' (hopper_move_generator knight_table (-1))  to_center
bBishop = bPiece   350 'B' (slider_move_generator bishop_table (-1))  to_center
bRook   = bPiece   500 'R' (slider_move_generator rook_table (-1))  to_center
bQueen  = bPiece   900 'Q' (slider_move_generator queen_table (-1))  to_center
bKing   = bPiece 10000 'K' (hopper_move_generator king_table (-1))  flee_center

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
             "pppppppp" ++
             "rnbqkbnr"
  
startboard :: (Array Int Piece)
startboard = array (0,63) $ zip [0..63] (map char2piece startBoard)
 
movegen board = 
  let s = (to_move board)
      b = (squares board) in
  concat $ map (\(sq,pc)->if s==(side pc) then (generator pc) b sq else [] ) (assocs b)
  
print_board b = do
  putStrLn $ unlines $ [print_horiz]++(concat (map print_rank [0..7])) where
    print_rank r = [(concat (map (\f->"| "++(show (b ! (r*8+f))) ++ " ") [0..7])) ++ "|",print_horiz]
    print_horiz = "+" ++ ( concat (replicate 8 "---+"))

board_material ba = foldl (\cv pc->cv + (value pc)*(side pc)) 0 (elems ba)

pb _ = print_board startboard

sb = Board startboard 1 []

--
-- tree search 
--

data SearchTree = SearchTree { board      :: Board , 
                               staticEval :: Int , 
                               childBoxes :: [(Move,SearchTree)] }

-- the search tree is infinite; ram is not. 
makeSearchTree board = SearchTree board (eval board) (map (\move->(move,makeSearchTree ((player move) board))) (movegen board))

negamax tree depth alpha beta = 
  if depth<=0 then ((staticEval tree)*(to_move (board tree)) , []) else 
  chess_foldl check_child (-99999,[]) (childBoxes tree) where 
    chess_foldl fn a [] = a
    chess_foldl fn (alpha',ml) (f:r) = if alpha'>=beta then (alpha',ml) else chess_foldl fn (fn (alpha',ml) f) r
    check_child (best_score,bestmove) (move,subtree) =
      let (alpha',child_line)=neg (negamax subtree (depth-1) (-beta) (-alpha) ) in 
        if alpha'>best_score then (alpha',move:child_line) else (best_score,bestmove) 
    neg (s,m)=(-s,m)

--mtdf -- do zero width serches until we zoom in on a good value
mtdf sb depth =
  mtdf_loop (-99999) 99999 where 
    tree  = makeSearchTree sb
    mtdf_loop low high = 
      let guess       = (low+high) `div` 2
          (score,line)= negamax tree depth guess (guess+1) in
      if (low+1)>=high then (score,line) else 
        if score<guess then (mtdf_loop low guess) else (mtdf_loop guess high)

main = print $ mtdf sb 5 