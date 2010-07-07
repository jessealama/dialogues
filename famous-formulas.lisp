;;; famous-formulas.lisp Some interesting/famous formulas

(in-package :dialogues)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((p (make-atomic-formula 'p))
	(q (make-atomic-formula 'q))
	(r (make-atomic-formula 'r)))
    
    (defparameter peirce-formula 
      (-> (-> (-> p q) p) p))

    (defparameter excluded-middle 
      (v p (¬ p)))

    (defparameter dummett-formula 
      (v (-> p q) (-> q p)))

    (defparameter double-negation-intro
      (-> p
	  (¬ (¬ p))))
    
    (defparameter double-negation-elimination
      (-> (¬ (¬ p)) p))
    
    (defparameter k-formula
      (-> p (-> q p)))
    
    (defparameter b-formula
      (->
       (-> p q)
       (->
	(-> r p)
	(-> r q))))

    (defparameter c-formula
      (->
       (-> p
	   (-> q r))
       (-> q
	   (-> p r))))

    (defparameter w-formula 
      (->
       (-> p
	   (-> p q))
       (-> p q)))

    (defparameter weak-excluded-middle
      (v
       (¬ p)
       (¬ (¬ p))))
    
    (defparameter scott-formula 
      (-> (-> (-> (¬ (¬ p)) p) (v p (¬ p)))
	  (v (¬ p) (¬ (¬ p)))))

    (defparameter smetanich-formula 
      (-> (-> (¬ q) p) 
	  (-> (-> (-> p q) p) p)))
    
    (defparameter de-morgan-not-and-implies-or
      (-> (¬ (& p q))
	  (v (¬ p) (¬ q))))

    (defparameter de-morgan-not-or-implies-and
      (-> (¬ (v p q))
	  (& (¬ p) (¬ q))))

    (defparameter de-morgan-and-not-implies-not-or
      (-> (& (¬ p) (¬ q))
	  (¬ (v p q))))

    (defparameter de-morgan-or-not-implies-not-and
      (-> (v (¬ p) (¬ q))
	  (¬ (& p q))))

    ))

;;; famous-formulas.lisp ends here