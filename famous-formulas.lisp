;;; famous-formulas.lisp Some interesting/famous formulas

(in-package :dialogues)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((p (make-atomic-formula 'p))
	(q (make-atomic-formula 'q))
	(r (make-atomic-formula 'r))
	(s (make-atomic-formula 's)))
    
    (defconstant peirce-formula 
      (-> (-> (-> p q) p) p))

    (defconstant excluded-middle 
      (v p (¬ p)))

    (defconstant dummett-formula 
      (v (-> p q) (-> q p)))

    (defconstant double-negation-intro
      (-> p
	  (¬ (¬ p))))
    
    (defconstant double-negation-elimination
      (-> (¬ (¬ p)) p))
    
    (defconstant k-formula
      (-> p (-> q p)))
    
    (defconstant b-formula
      (->
       (-> p q)
       (->
	(-> r p)
	(-> r q))))

    (defconstant c-formula
      (->
       (-> p
	   (-> q r))
       (-> q
	   (-> p r))))

    (defconstant w-formula 
      (->
       (-> p
	   (-> p q))
       (-> p q)))

    (defconstant weak-excluded-middle
      (v
       (¬ p)
       (¬ (¬ p))))
    
    (defconstant scott-formula 
      (-> (-> double-negation-elimination excluded-middle)
	  weak-excluded-middle))

    (defconstant smetanich-formula 
      (-> (-> (¬ q) p) peirce-formula))
    
    (defconstant de-morgan-not-and-implies-or
      (-> (¬ (& p q))
	  (v (¬ p) (¬ q))))

    (defconstant de-morgan-not-or-implies-and
      (-> (¬ (v p q))
	  (& (¬ p) (¬ q))))

    (defconstant de-morgan-and-not-implies-not-or
      (-> (& (¬ p) (¬ q))
	  (¬ (v p q))))

    (defconstant de-morgan-or-not-implies-not-and
      (-> (v (¬ p) (¬ q))
	  (¬ (& p q))))

    ))

;;; famous-formulas.lisp ends here