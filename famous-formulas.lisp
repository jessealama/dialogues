;;; famous-formulas.lisp Some interesting/famous formulas

(in-package :dialogues)

(defmacro define-famous-formula (name definition)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,name ,definition)
       (defmethod form->formula ((op (eql ',name)))
	 ,name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((p (make-atomic-formula 'p))
	(q (make-atomic-formula 'q))
	(r (make-atomic-formula 'r)))
    
    (define-famous-formula peirce-formula
	(-> (-> (-> p q) p) p))
    
    (define-famous-formula excluded-middle 
	(v p (¬ p)))
    
    (define-famous-formula dummett-formula 
	(v (-> p q) (-> q p)))
    
    (define-famous-formula double-negation-introduction
	(-> p
	    (¬ (¬ p))))
    
    (define-famous-formula double-negation-elimination
	(-> (¬ (¬ p)) p))
    
    (define-famous-formula k-formula
	(-> p (-> q p)))
    
    (define-famous-formula b-formula
	(->
	 (-> p q)
	 (->
	  (-> r p)
	  (-> r q))))
    
    (define-famous-formula c-formula
	(->
	 (-> p
	     (-> q r))
	 (-> q
	     (-> p r))))

    (define-famous-formula w-formula 
	(->
	 (-> p
	     (-> p q))
	 (-> p q)))

    (define-famous-formula weak-excluded-middle
	(v
	 (¬ p)
	 (¬ (¬ p))))
    
    (define-famous-formula scott-formula 
	(-> (-> (-> (¬ (¬ p)) p) (v p (¬ p)))
	    (v (¬ p) (¬ (¬ p)))))

    (define-famous-formula smetanich-formula 
	(-> (-> (¬ q) p) 
	    (-> (-> (-> p q) p) p)))
    
    (define-famous-formula de-morgan-not-and-implies-or-not
	(-> (¬ (& p q))
	    (v (¬ p) (¬ q))))

    (define-famous-formula de-morgan-not-or-implies-and-not
	(-> (¬ (v p q))
	    (& (¬ p) (¬ q))))

    (define-famous-formula de-morgan-and-not-implies-not-or
	(-> (& (¬ p) (¬ q))
	    (¬ (v p q))))

    (define-famous-formula de-morgan-or-not-implies-not-and
	(-> (v (¬ p) (¬ q))
	    (¬ (& p q))))

    (define-famous-formula anti-connexive-formula
	(v (-> p (¬ p))
	   (-> (¬ p) p)))

    (define-famous-formula ex-falso-quodlibet
	(-> (& p (¬ p))
	    q))

    (define-famous-formula aristotles-thesis-positive-antecedent
	(¬ (-> p (¬ p))))

    (define-famous-formula aristotles-thesis-negative-antecedent
	(¬ (-> (¬ p) p)))

    (define-famous-formula conditional-excluded-middle
	(v (-> p q) (-> p (¬ q))))

    (define-famous-formula implicational-ex-falso
	(-> (¬ p) (-> p q)))

    ))

;;; famous-formulas.lisp ends here