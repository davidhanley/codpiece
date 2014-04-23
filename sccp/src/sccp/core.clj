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

(defrecord piece [side name value generator evaluator])

(defrecord move [from to player])

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

(defn hopper-moves[ deltas coord ] 
  (map (fn [to] (make-simple-move coord to)) (hopper-to coord deltas)))

(def knight-moves (map (partial hopper-moves knight-deltas) squares-coords))

(def wpawn   (piece. 1 " p" 100 nil nil))
(def wknight (piece. 1 " n" 325 nil nil))
(def wbishop (piece. 1 " b" 350 nil nil))
(def wrook   (piece. 1 " r" 500 nil nil))
(def wqueen  (piece. 1 " q" 900 nil nil))
(def wking   (piece. 1 " k" 10000 nil nil))
/
(def bpawn   (piece. -1 " P" -100 nil nil))
(def bknight (piece. -1 " N" -325 nil nil))
(def bbishop (piece. -1 " B" -350 nil nil))
(def brook   (piece. -1 " R" -500 nil nil))
(def bqueen  (piece. -1 " Q" -900 nil nil))
(def bking   (piece. -1 " K" -10000 nil nil))

(def pieces [none wpawn wknight wbishop wrook wqueen wking 
                  bpawn bknight bbishop brook bqueen bking])

(defn char-piece[ch] (first (filter (fn[pc](= ch (get (.name pc) 1))) pieces)))

;(def wpiece (partial piece. 1))
;(def bpiece (partial piece. -1))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Let's play chess!!"))
