(ns sccp.core
    (:gen-class))

(use '[clojure.string :only (join split upper-case)])

(defn l[] (use 'sccp.core :reload))

(def white 1)
(def black -1)

(def cols "abcdefgh")
(def rows "87654321")

(defn square-to-cr[ sq ] [ (mod sq 8) , (quot sq 8) ])
(defn cr-to-square[ [c r] ] (+ c (* r 8)))

(defn square-to-string[ sq ]
  (if (or (not sq)(< sq 0)(> sq 63)) "none"
    (let [[c r](square-to-cr sq)]
	 (str (get cols c) (get rows r)))))

(def squares (vec (range 0 64)))
(def squares-coords (map square-to-cr squares))

(defn string-to-square[ str ](.indexOf (vec (map square-to-string squares)) str))

;cute hack to make constants for all the chess squares
(doseq [sq squares](eval (list 'def (symbol (square-to-string sq)) sq)))

(defrecord piece [side name value generator evaluator])

(defrecord move  [from to move-assoc extra-assoc])

(defrecord board [squares 
                  to-move 
		  white-king black-king 
		  en-pesant
	          wcq wck bcq bck
                  drawing-moves 
		  half-moves])

(def none (piece. 0 "  " 0 #() #(0)))

(defn play[ bd move ]
  (let [s1 (:squares bd)
        delta (concat [(:from move) none (:to move) (s1 (:from move))] (:move-assoc move))
        s2 (apply assoc s1 delta)
	ed (concat ['squares s2] [:to-move (- (:to-move bd)) ] (:extra-assoc move)) ]
      (apply assoc bd :squares s2 :half-moves (inc (:half-moves bd)) :en-pesant nil ed)))
	  

(defn move-to-string[ x ] (let [e (:extra-text x)]
  (str (square-to-string (:from x)) "-" (square-to-string (:to x)) (if e e ""))))
	
;(defmethod print-method move [x ^java.io.Writer writer]
;  (print-method (move-to-string x) writer))

(def axis (range 8))
(defn axis-ok[ a ]     (and (>= a 0) (< a 8)))
(defn coord-ok[ [c r] ](and (axis-ok c)(axis-ok r)))


(def knight-deltas [ [-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1] ] )
(def king-deltas   [ [-1 -1] [0 -1] [1 -1]  [-1 0] [1 0] [-1 1] [0 1] [1 1] ] )

(defn add-coord [ [c1 r1] [c2 r2] ]  [ (+ c1 c2) (+ r1 r2) ] )

(defn hopper-to[ coord deltas ] (filter coord-ok (map (partial add-coord coord) deltas)))

(defn simple-play[ f t ]
  (fn [board] (assoc board f none t (board f))))

(declare wking wqueen wrook wbishop wknight wpawn  
         bking bqueen brook bbishop bknight bpawn)

(defn de-coord[ vec ](mapv (fn[x](if (vector? x) (cr-to-square x) x)) vec))

(defn make-simple-move[ from-coord to-coord &{ :keys [extra-board-change extra-struct-change] } ]
  (let [f (cr-to-square from-coord) t (cr-to-square to-coord) ]
       (move. f t 
	      (de-coord extra-board-change)
	      (concat (when (or (= f e1)(= f h1)(= t h1)) ['wkc false])
		      (when (or (= f e1)(= f a1)(= f a1)) ['wqc false])
		      (when (or (= f e8)(= f h8)(= t h8)) ['bkc false])
		      (when (or (= f e8)(= f a8)(= t h8)) ['bqc false])
		      (de-coord extra-struct-change)
		      ))))

;
; Make moves for the "hopping" pieces such as knights and kings

(defn hopper-moves[ deltas coord  ] 
  (mapv (fn [to] (make-simple-move coord to)) (hopper-to coord deltas)))

(def knight-moves (mapv (partial hopper-moves knight-deltas) squares-coords))

(defn hops-where-dest-not-side[ table side ]
  (fn[board board-squares sq](filter (fn[mv](not= (:side (board-squares (:to mv))) side)) (table sq))))	
			
(def knight-gen (partial hops-where-dest-not-side knight-moves))

;
; Now make moves for the "sliding" pieces such as bishops, rooks, queens

(def rook-deltas   [[0 -1] [-1 0] [1 0] [0 1]])
(def bishop-deltas [[-1 -1][-1 1][1 -1][1 1]])

(defn trace-ray[ start direc ]
  (when (coord-ok start) (cons start (trace-ray (add-coord start direc) direc))))

(defn moves-on-ray[ start direc ] 
  (map (fn[ dest ](make-simple-move start dest)) (rest (trace-ray start direc))))

(defn make-ray-lookup[ deltas ] 
  (mapv (fn[sq](filter (fn[l](not= (count l) 0)) (mapv (fn[delt](moves-on-ray sq delt)) deltas))) squares-coords))

(defn ray-until-side[ board ray other ]
  (when (not (empty? ray))
    (let [move (first ray)
	  there (:side (board (:to move)))]
	  (if (= there 0) (cons move (ray-until-side board (rest ray) other))
	   (if (= there other) [move] [])))))

(def rook-table (make-ray-lookup rook-deltas))
(def bishop-table (make-ray-lookup bishop-deltas))
(def queen-table (vec (map concat rook-table bishop-table)))

(defn slider-gen[ raytable other ]
  (fn[board board-squares sq](mapcat (fn[ray](ray-until-side board-squares ray other)) (raytable sq))))

;
; Pawns just have to be difficult! 
; 3 pawn move vectors per square, one for forward moves, one for captures, one for en pesants 

(defn promotify[ pawn-moves ]
  (mapcat (fn[move](let [to (:to move)
		         func (fn[pc] (assoc move :move-assoc [to pc] :extra-text (str "=" (subs (:name pc) 1))) ) ]
	    (cond
	     (< to a7) (map func [wqueen wrook wbishop wknight])
	     (> to h2) (map func [bqueen brook bbishop bknight])
	     :else [move]))) pawn-moves))	     
		   
(defn pawn-moves[ from direc ]
  (let [
    [col row] from
    pawnmove (fn[ to & r ](apply make-simple-move from to r))
    simple (cons (pawnmove (add-coord from [0 direc]))
		       (when (or (and (= direc -1) (= row 6))
			         (and (= direc 1)  (= row 1)))
			   [(pawnmove (add-coord from [0 (* 2 direc)]) :extra-struct-change [:en-pesant (add-coord from [0 direc])])]))
    not-left-border (not= (first from) 0)
    not-right-border (not= (first from) 7)
    left-cap (add-coord from [-1 direc]) 
    right-cap (add-coord from [1 direc])
    capts (concat (when not-left-border  [(pawnmove left-cap)])
		  (when not-right-border [(pawnmove right-cap)] )) 
    ep-move (fn[to][(pawnmove to :extra-board-change [[(first to)(second from)] none] )]) ; :extra-text "ep"
    eps (when (or (and (= row 3)(= direc -1))
		  (and (= row 4)(= direc 1)))
	  (concat (when not-left-border (ep-move left-cap))
		  (when not-right-border (ep-move right-cap))))	  ]
    (if (or (= row 0) (= row 7)) [] (list (promotify simple) (promotify capts) eps))))
 
(defn pawn-move-generator[ capturing table ]
  (fn[ board board-squares sq ] (let [
		    pawn-moves (table sq)
		    [moves captures en-pesants] pawn-moves]
		    (concat
		     (take-while (fn[mv](= (board-squares (:to mv)) none)) moves)
		     (filter (fn[mv](= (:side (board-squares (:to mv))) capturing)) captures)
		     (filter (fn[mv](= (:to mv) (:en-pesant board))) en-pesants)
		     ))))

(def white-pawn-moves (mapv (fn[sq](pawn-moves sq -1)) squares-coords))
(def black-pawn-moves (mapv (fn[sq](pawn-moves sq  1)) squares-coords))

(def white-pawn-captures (mapv second white-pawn-moves))
(def black-pawn-captures (mapv second black-pawn-moves))

(defn add-assoc-to-slot[ struct slot extra ] (assoc struct slot (concat (slot struct) extra)))

(defn make-king-move[ king sq ] 
  (mapv (fn[mv] (add-assoc-to-slot mv :extra-assoc [king (:to mv)])) (hopper-moves king-deltas sq)))
 
(def white-king-moves (mapv (partial make-king-move :white-king) squares-coords))
(def black-king-moves (mapv (partial make-king-move :black-king) squares-coords))

; we need to discover if a square is attacked 

(defn move-ray-ends-with[ squares ray pcs ]
  (if (= ray []) false
    (let [mv (first ray)
	  target (squares (:to mv)) ]
	  (if (= target none) (move-ray-ends-with squares (rest ray) pcs)
	    (not= (.indexOf pcs target) -1)))))

(defn attacks-with[ board sq diag-sliders vert-sliders knight king pawn pawn-captures ]
  (let [squares (:squares board)
        slider-attacks (fn[ table pcs](some (fn[ ray ]( move-ray-ends-with squares ray pcs)) (table sq)))
	hopper-attacks (fn[table pc](some (fn[ mv ](= (squares (:to mv)) pc)) (table sq))) ]
       (or (slider-attacks bishop-table  diag-sliders)
	   (slider-attacks rook-table    vert-sliders)
	   (hopper-attacks knight-moves  knight)
	   (hopper-attacks white-king-moves    king)
	   (hopper-attacks pawn-captures pawn))))

(defn white-attacks[ board sq ]
  (attacks-with board sq [wbishop wqueen] [wrook wqueen] wknight wking wpawn black-pawn-captures))

(defn black-attacks[ board sq ]
  (attacks-with board sq [bbishop bqueen] [brook bqueen] bknight bking bpawn white-pawn-captures))

(defn to-move-can-capture-king[ board ]
  ;(assert (= (:white-king board) (.indexOf (:squares board) wking)))
  ;(assert (= (:black-king board) (.indexOf (:squares board) bking)))
  (if (= (:to-move board) black)
      (black-attacks board (:white-king board))
      (white-attacks board (:black-king board))))

(def white-kingside-castle (make-simple-move [4 7] [6 7] :extra-board-change [h1 none f1 wrook g1 wking]))
(def white-queenside-castle (make-simple-move [4 7] [2 7] :extra-board-change [a1 none c1 wking d1 wrook]))
(def black-kingside-castle (make-simple-move [4 0] [6 0] :extra-board-change [h8 none f8 brook g8 bking]))
(def black-queenside-castle (make-simple-move [4 0] [2 0] :extra-board-change [a1 none c8 bking d8 brook]))

(defn white-castles[board bs sq]
  (let [empty? (fn[sq](= (bs sq) none))]
       (concat
	(when (and (:wkc board) (empty? f1) (empty? g1)) [white-kingside-castle])
	(when (and (:wkc board) (empty? d1) (empty? c1) (empty? b1)) [white-queenside-castle]))))

(def white-king-gen (hops-where-dest-not-side white-king-moves white))
(def black-king-gen (hops-where-dest-not-side black-king-moves black))

(def wknight (piece. 1 " N" 325 (knight-gen white) nil))
(def wbishop (piece. 1 " B" 350 (slider-gen bishop-table -1) nil))
(def wrook   (piece. 1 " R" 500 (slider-gen rook-table -1) nil))
(def wqueen  (piece. 1 " Q" 900 (slider-gen queen-table -1) nil))
(def wking   (piece. 1 " K" 10000 white-king-gen nil))


(def bknight (piece. -1 " n" -325 (knight-gen black) nil))
(def bbishop (piece. -1 " b" -350 (slider-gen bishop-table 1) nil))
(def brook   (piece. -1 " r" -500 (slider-gen rook-table 1) nil))
(def bqueen  (piece. -1 " q" -900 (slider-gen queen-table 1) nil))
(def bking   (piece. -1 " k" -10000 black-king-gen nil))
       
(def bpawn   (piece. -1 " p" -100 (pawn-move-generator 1 black-pawn-moves) nil))
(def wpawn   (piece. 1 " P" 100 (pawn-move-generator -1 white-pawn-moves) nil))

(def pieces [none wpawn wknight wbishop wrook wqueen wking 
                  bpawn bknight bbishop brook bqueen bking])

(defn char-piece[ch] (first (filter (fn[pc](= ch (get (:name pc) 1))) pieces)))

(def fen-start "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" )

(defn fen-part[ ch ]
  (let [pc (char-piece ch)]
       (if pc [pc]
	 (if (>= (.indexOf "12345678" (str ch)) 0)
	     (repeat (read-string (str ch)) none)
	   []))))
      
(defn parse-int[x]
  (try (Integer. x) (catch Exception e 0)))

(defn parse-fen[ fen-string ]
  (let [[squares to-move castling ep draw-moves half-moves] (split fen-string #" ")
        board-squares (vec (mapcat fen-part squares)) 
	c (fn[ch](not (not (some #{ch} castling)))) 
	where (fn[pc](.indexOf board-squares pc))]
	(board. board-squares
		(if (= to-move "w") white black)
		(where wking) (where bking)  (string-to-square ep)
		(c \K) (c \Q) (c \k) (c \q)  
		(parse-int draw-moves) (parse-int half-moves))))
		
(def start-board (parse-fen fen-start))      

(defn print-board[ board ]
  (let [div "\n+---+---+---+---+---+---+---+---+\n"
        pbs (fn[[c r]](str (when (= c 0) (str div "|")) (:name (board (cr-to-square [c r]))) " |" )) ] 
  (join (conj (mapv pbs squares-coords) div))))
		  
(declare generate-moves)
   
(defmethod print-method board [x ^java.io.Writer writer]
  (let [w (fn[ & pcs ](let [l (apply str (conj pcs "\n"))](.write writer l)))]
       (w (print-board (:squares x)))
       (w "to move                 : " (if (= (:to-move x) white) "white" "black"))
       (w "moves                   : " (vec (generate-moves x)))
       (w "Kings at                : " (mapv square-to-string [(:white-king x)(:black-king x)]))
       (w "En pesant               : " (square-to-string (:en-pesant x)))
       (w "Castles wcq wck bcq bck : " (mapv (fn[f](f x)) [:wcq :wck :bcq :bck]))
       (w "at move                 : " (:half-moves x))
       (w "drawbreak moves         : " (:drawing-moves x))
       ))

;(defmethod print-method move [x ^java.io.Writer writer]
;  (print-method (str (square-to-string (:from x)) "-" (square-to-string (:to x))) writer))
  
        
(defn material-balance[ bd ](reduce + (map :value bd)))
(defn locate[ piece board ](.indexOf (:squares board) piece))

(defn generate-moves[ board ]
  (let [bs (:squares board)
        for-side (:to-move board)]
       (mapcat (fn[pc sq](if (= for-side (:side pc)) ((:generator pc) board bs sq) [])) bs squares)))

(defn perft[ depth bd ]
  (if (to-move-can-capture-king bd)
      0 
    (if (= depth 0) 1
      (reduce + (map (fn[mv](perft (dec depth) (play bd mv))) (generate-moves bd))))))

(defn string-to-move[ line board ]
  (first (filter (fn[mv](= (move-to-string mv) line)) (generate-moves board))))

(defn main-loop[ board ]
  (print board)
  (flush)
  (let [mv (string-to-move (read-line) board)]
       (recur (if mv (play board mv) board))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (main-loop start-board))

