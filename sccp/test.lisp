(defun l() (load "test.lisp"))

(defun fact(x)(if (= x 1) 1 (* x (fact (- x 1)))))
(defun choose(n m)(/ (fact n)(fact m)))
(defun binom-prob(n m p)(* (choose n m)(expt p m)(expt (- 1 p)(- n m))))

(defun ans(x)
  (/
   (* (1+ (expt 3 (- 60 x)))(fact 60)(fact (- 60 x)))
   (* (fact x)(expt 4 60))))

(defun sim( size opts )
  (let ((hits 0))
    (loop for a from 1 to size do
      (when (zerop (random opts))
	  (incf hits))) hits))

(defun guesser()
  (let ((h 0)(reps 100000000))
  (loop for a from 1 to reps do
  (let ((res (sim 60 4)))
    ;(format t "~d " res)
    (when (= res 10)
      (incf h)
      ;(format t "*")
      )))
  (format t "Times : ~d (~d)" h (float (* (/ h reps) 100)))))