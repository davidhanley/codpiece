
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

wPiece = Piece 1
bPiece = Piece (-1)

dummy_generator::  Array Int Piece -> Int -> [Move]
dummy_generator a i = []

empty   = Piece 0 0 ' ' dummy_generator

axis_ok x               = x >= 0 && x < 8
coord_ok ( r , f )      = axis_ok r && axis_ok f
--square_to_coord :: Int -> ( Int , Int )
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
generate_slider sq =  map (generate_ray sq)

--the rays along which bishops and rooks can move 
(rook_deltas,bishop_deltas) = partition (\(x,y)->x==0||y==0) king_deltas 

make_lookup_table cgf = array (0,63) $ zip squares (map cgf squares)

knight_table = make_lookup_table (generate_hopper knight_deltas)

hopper_move_generator ::  Array Int [Move] -> Int -> Array Int Piece -> Int -> [Move]
hopper_move_generator table my_side board square =
  filter (\move->(side (board ! (to move))) /= my_side) (table ! square)

wPawn   = wPiece 100 'p' dummy_generator 
wKnight = wPiece 325 'n' $ hopper_move_generator knight_table 1
wBishop = wPiece 350 'b' dummy_generator
wRook   = wPiece 500 'r' dummy_generator
wQueen  = wPiece 900 'q' dummy_generator
wKing   = wPiece 10000 'k' dummy_generator

bPawn   = bPiece 100 'P' dummy_generator
bKnight = bPiece 325 'N' $ hopper_move_generator knight_table (-1)
bBishop = bPiece 350 'B' dummy_generator
bRook   = bPiece 500 'R' dummy_generator
bQueen  = bPiece 900 'Q' dummy_generator
bKing   = bPiece 10000 'K' dummy_generator

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
