;;; famous-formulas.lisp Some interesting, famous formulas

(in-package :dialogues)

(defconstant peirce-formula 
    (make-implication (make-implication (make-implication 'p 'q) 'p) 'p))

(defconstant excluded-middle 
    (make-binary-disjunction 'p (negate 'p)))

(defconstant dummett-formula 
    (make-binary-disjunction (make-implication 'p 'q)
			     (make-implication 'q 'p)))

(defconstant markov-formula 
    (make-implication (negate (negate 'p)) 
		      'p))

(defconstant double-negation-intro
    (make-implication 'p
		      (negate (negate 'p))))

(defconstant double-negation-elimination
    (make-implication (negate (negate 'p))
		      'p))

(defconstant k-formula
    (make-implication 'p
		      (make-implication 'q 'p)))

(defconstant b-formula
    (make-implication
     (make-implication 'p 'q)
     (make-implication
      (make-implication 'r 'p)
      (make-implication 'r 'q))))

(defconstant c-formula
    (make-implication
     (make-implication 'p
		       (make-implication 'q 'r))
     (make-implication 'q
		       (make-implication 'p 'r))))

(defconstant w-formula 
    (make-implication
     (make-implication 'p
		       (make-implication 'p 'q))
     (make-implication 'p 'q)))

(defconstant weak-excluded-middle
    (make-binary-disjunction
     (negate 'p)
     (negate (negate 'p))))

(defconstant scott-formula 
    (make-implication (make-implication double-negation-elimination
					excluded-middle)
		      weak-excluded-middle))

(defconstant smetanich-formula 
    (make-implication (make-implication (negate 'q) 'p)
		      peirce-formula))

;;; famous-formulas.lisp ends here