open Sccp
open Printf

(* create the SQL entries for piece hash codes *)
let in_hash piece sq = iexec (sprintf "insert into hashes values('%s',%d,%d)" 
     piece.glyph sq (Random.bits()))

let make_hashes() = 
  List.iter (fun piece->List.iter (fun sq->in_hash piece sq) x88_squares ) pieces

let strtok = Str.split (Str.regexp "[ \t\r\n]+")

let read_file_into fn func =
  let o = open_in fn in
  let readit () = while true do func (input_line o)  done in
    try readit() with End_of_file -> close_in o

(* populate the opening book database tables *)

let rec find_in_book bb = 
  match (llexec (sprintf "select id from book_positions where board='%s'" bb)) with
  [[bb]]->bb |
  _ -> (iexec (sprintf "insert into book_positions (board) values('%s')" bb);
       find_in_book bb)

let rec put_in_book bb bm = 
  (*print_chess_board();
  printf "move : %s\n" bm*)
  iexec (sprintf "insert into book_moves values(%s,'%s')" (find_in_book bb) bm)

let read_book_into_db book_name = 
  read_file_into book_name 
   (fun line->printf "Lines: %s\n" line; let moves = 
              List.map (fun sm->match (string_to_move sm) with
                                Some(mv)->ignore(play mv); mv | 
                                None->print_chess_board(); printf "%s" sm; raise (Chess_error "Bad book")) (strtok line) in
              List.iter (fun f->unplay f; put_in_book (board_to_string()) (move_to_string f))
                                (List.rev moves))

(*
let study() = 
  lexec "select position,depth from brain order by nodes_searched limit 1"
    (fun l->match l with
             [pos;depth]->let new_depth=1+int_of_string depth in
               match (search new_depth) with 
                _,move::_ -> iexec( sprintf "update brain set move='%s' and depth='%d' where position='%s'" (move_to_string move) new_depth pos))

                
let _ = study()
*)
