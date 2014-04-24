(ns sccp.core
  (:gen-class))

(def white 1)
(def black -1)

(def cols "abcdefgh")
(def rows "87654321")

(defn square-to-cr[ sq ] [ (mod sq 8) , (quot sq 8) ])
(defn cr-to-square[ [c r] ] (+ c (* r 8)))

(defn square-to-string[ sq ]
  (let [[c r](square-to-cr sq)]
       (str (get cols c) (get rows r))))

(def squares (vec (range 0 64)))
(def squares-coords (map square-to-cr squares))

;cute hack to make constants for all the chess squares
(defn squarify[sq](eval (list 'def (symbol (square-to-string sq)) sq)))
(doall (map squarify squares))

(defrecord piece [side name value generator evaluator])

(defrecord move [from to player])

(defmethod print-method move [x ^java.io.Writer writer]
  (print-method (str (square-to-string (:from x)) "-" (square-to-string (:to x))) writer))

(defn axis-ok[ a ]     (and (>= a 0) (< a 8)))
(defn coord-ok[ [c r] ](and (axis-ok c)(axis-ok r)))


(def knight-deltas [ [-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1] ] )
(def king-deltas   [ [-1 -1] [0 -1] [1 -1]  [-1 0] [1 0] [-1 1] [0 1] [1 1] ] )

(defn add-coord [ [c1 r1] [c2 r2] ]  [ (+ c1 c2) (+ r1 r2) ] )

(defn hopper-to[ coord deltas ] (filter coord-ok (map (partial add-coord coord) deltas)))

(def none (piece. 0 "  " 0 #() #(0)))

(defn simple-play[ f t ]
  (fn [board] (assoc board none f (get (board f)) t)))

(defn make-simple-move[ from-coord to-coord ]
  (let [f (cr-to-square from-coord) t (cr-to-square to-coord) ]
       (move. f t (simple-play f t))))

;
; Make moves for the "hopping" pieces such as knights and kings

(defn hopper-moves[ deltas coord ] 
  (map (fn [to] (make-simple-move coord to)) (hopper-to coord deltas)))

(def knight-moves (vec (map (partial hopper-moves knight-deltas) squares-coords)))

(defn hops-where-dest-not-side[ table side ]
  (fn[board sq](filter (fn[mv](not= (:side (board (:to mv))) side)) (table sq))))	
			
(def knight-gen (partial hops-where-dest-not-side knight-moves))

(def king-moves (vec (map (partial hopper-moves king-deltas) squares-coords)))

(def king-gen (partial hops-where-dest-not-side king-moves))

;
; Now make moves for the "sliding" pieces such as bishops, rooks, queens

(def rook-deltas   [[0 -1] [-1 0] [1 0] [0 1]])
(def bishop-deltas [[-1 -1][-1 1][1 -1][1 1]])

(defn trace-ray[ start direc ]
  (when (coord-ok start) (cons start (trace-ray (add-coord start direc) direc))))

(defn moves-on-ray[ start direc ] 
  (map (fn[ dest ](make-simple-move start dest)) (rest (trace-ray start direc))))

(defn make-ray-lookup[ deltas ] 
  (map (fn[sq](map (fn[delt](moves-on-ray sq delt)) deltas)) squares-coords))

(defn dummy[bd sq][])

(def wpawn   (piece. 1 " p" 100 dummy nil))
(def wknight (piece. 1 " n" 325 (knight-gen white) nil))
(def wbishop (piece. 1 " b" 350 dummy nil))
(def wrook   (piece. 1 " r" 500 dummy nil))
(def wqueen  (piece. 1 " q" 900 dummy nil))
(def wking   (piece. 1 " k" 10000 (king-gen white) nil))
/
(def bpawn   (piece. -1 " P" -100 nil nil))
(def bknight (piece. -1 " N" -325 (knight-gen black) nil))
(def bbishop (piece. -1 " B" -350 nil nil))
(def brook   (piece. -1 " R" -500 nil nil))
(def bqueen  (piece. -1 " Q" -900 nil nil))
(def bking   (piece. -1 " K" -10000 (king-gen black) nil))

(def pieces [none wpawn wknight wbishop wrook wqueen wking 
                  bpawn bknight bbishop brook bqueen bking])

(defn char-piece[ch] (first (filter (fn[pc](= ch (get (.name pc) 1))) pieces)))

(def start-board (vec (map char-piece 
     (str "RNBQKBNR"
     	  "PPPPPPPP"
          "        "
	  "        "
	  "        "
          "        "
          "pppppppp"
	  "rnbqkbnr"))))

(defn material-balance[ bd ](reduce + (map :value bd)))

(defn generate-moves[ bd for-side ]
  (mapcat (fn[pc sq](if (= for-side (:side pc)) ((:generator pc) bd sq) [])) bd squares))

;(def wpiece (partial piece. 1))
;(def bpiece (partial piece. -1))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Let's play chess!!"))
