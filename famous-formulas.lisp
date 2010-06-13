;;; famous-formulas.lisp Some interesting, famous formulas

(in-package :dialogues)

(defconstant-if-unbound peirce-formula 
  (make-implication (make-implication (make-implication 'p 'q) 'p) 'p))

(defconstant-if-unbound excluded-middle 
    (make-binary-disjunction 'p (negate 'p)))

(defconstant-if-unbound dummett-formula 
    (make-binary-disjunction (make-implication 'p 'q)
			     (make-implication 'q 'p)))

(defconstant-if-unbound markov-formula 
    (make-implication (negate (negate 'p)) 
		      'p))

(defconstant-if-unbound double-negation-intro
    (make-implication 'p
		      (negate (negate 'p))))

(defconstant-if-unbound double-negation-elimination
    (make-implication (negate (negate 'p))
		      'p))

(defconstant-if-unbound k-formula
    (make-implication 'p
		      (make-implication 'q 'p)))

(defconstant-if-unbound b-formula
    (make-implication
     (make-implication 'p 'q)
     (make-implication
      (make-implication 'r 'p)
      (make-implication 'r 'q))))

(defconstant-if-unbound c-formula
    (make-implication
     (make-implication 'p
		       (make-implication 'q 'r))
     (make-implication 'q
		       (make-implication 'p 'r))))

(defconstant-if-unbound w-formula 
    (make-implication
     (make-implication 'p
		       (make-implication 'p 'q))
     (make-implication 'p 'q)))

(defconstant-if-unbound weak-excluded-middle
    (make-binary-disjunction
     (negate 'p)
     (negate (negate 'p))))

(defconstant-if-unbound scott-formula 
    (make-implication (make-implication double-negation-elimination
					excluded-middle)
		      weak-excluded-middle))

(defconstant-if-unbound smetanich-formula 
    (make-implication (make-implication (negate 'q) 'p)
		      peirce-formula))

;;; famous-formulas.lisp ends here