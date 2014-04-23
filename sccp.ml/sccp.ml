(* SCCP.ML *)

(* First define types of peices, and squares, which is a piece and a color *)
type piece = None | Pawn | Knight | Bishop | Rook | Queen | King

(* a square is a piece, plus a color ( 1 for white, -1 for black ) *)
type square = piece * int

(* we address square by their rank and file *) 
type coord = int * int 

(* a quickie function to add coordinates *) 
let (+)  (r1,f1) (r2,f2) = r1+r2 , f1+f2

(* define the various kinds of chess moves *) 
type move = 
  Simple       of coord * coord |
  Pawn_ep      of coord * coord * coord |
  Castle       of coord * coord * coord * coord |
  Pawn_promote of coord * coord * square



(* access and update the board *) 
let set board c sq = board.(fst(c)).(snd(c)) <- sq
let get board c    = board.(fst(c)).(snd(c))

(* remove a piece at a coordinate, returning the removes piece *) 
let remove board c = let pc = get board c in set board c (None,0); pc

(* play a simple from , to move *) 
let splay board f t = set board t (remove board f)

(* given a board and a move update the board to reflect playing the move *) 
let play board move = 
  match move with 
    Simple(f,t)                -> splay board f t |
    Pawn_ep(f,t,e)             -> splay board f t ; remove board e; () |
    Castle(kf,kt,rf,rt)        -> splay board kf kt; splay board rf rt |
    Pawn_promote(f,t,promoter) -> remove board f; set board t promoter

(* Create an exception for an illegal move *) 
exception Illegal

(* call a function, adding the returned move if there is no exception *) 
let add_if func moves =
  try
    let move = func() in
      move::moves 
  with _ -> moves

(* given a "hopping" piece, and a list of deltas, return add the pseudolegal moves *) 
let rec gen_hopper board (piece,color) square deltas moves = 
  match deltas with 
    [] -> moves |
    delta::rest -> 
      gen_hopper board (piece,color) square rest (add_if (fun () -> let dest = square + delta in 
                                                       if snd(get board dest) = color then raise Illegal;
                                                       Simple(square,dest)) moves)
(* Generate a ray of moves for a sliding piece *) 
                                 
       
   
     