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
	(r (make-atomic-formula 'r))
	(s (make-atomic-formula 's)))
    
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

    (define-famous-formula wkp
	(-> (-> (¬ p)
		(v (¬ q) (¬ r)))
	    (v (-> (¬ p) (¬ q))
	       (-> (¬ p) (¬ r)))))

    (define-famous-formula modus-ponens
	(-> (& (-> p q) p) q))
    
    (define-famous-formula modus-tollens
	(-> (& (-> p q) (¬ q)) (¬ p)))

    (define-famous-formula hypothetical-syllogism
	(-> (& (-> p q) (-> q r))
	    (-> p r)))

    (define-famous-formula disjunctive-syllogism
	(-> (& (v p q) (¬ p)) (¬ q)))

    (define-famous-formula constructive-dilemma
	(-> (& (-> p q)
	       (-> r s))
	    (-> (v p r)
		(v q s))))

    (define-famous-formula destructive-dilemma
	(-> (& (-> p q)
	       (-> r s))
	    (-> (v (¬ r) (¬ s))
		(v (¬ p) (¬ q)))))

    (define-famous-formula conjunction-elimination
	(-> (& p q) p))

    (define-famous-formula conjunction-introduction
	(-> p (-> q (& p q))))

    (define-famous-formula disjunction-introduction
	(-> p (v p q)))

    (define-famous-formula composition
	(-> (& (-> p q)
	       (-> p r))
	    (-> p (& q r))))

    (define-famous-formula commutivity-of-conjunction
	(-> (& p q) (& q p)))

    (define-famous-formula commutativity-of-disjunction
	(-> (v p q) (v q p)))

    (define-famous-formula commutativity-of-implication
	(-> (-> p q) (-> q p)))

    (define-famous-formula associativity-of-conjunction
	(-> (& p (& q r)) (& (& p q) r)))

    (define-famous-formula associativity-of-disjunction
	(-> (v p (v p r)) (v (v p q) r)))

    (define-famous-formula associativity-of-implication
	(-> (-> p (-> q r))
	    (-> (-> p q) r)))

    (define-famous-formula distributivity-of-conjunction-over-disjunction-conjunctive-antecedent
	(-> (& p (v p r)) (v (& p q) (& p r))))

    (define-famous-formula distributivity-of-conjunction-over-disjunction-disjunctive-antecedent
	(-> (v (& p q) (& p r))
	    (& p (v q r))))

    (define-famous-formula distributivity-of-disjunction-over-conjunction-disjunctive-antecedent
	(-> (v p (& q r))
	    (& (v p q) (v p r))))

    (define-famous-formula distributivity-of-disjunction-over-conjunction-conjunctive-antecedent
	(-> (& (v p q) (v p r))
	    (v p (& q r))))

    (define-famous-formula transposition
	(-> (-> p q) (-> (¬ q) (¬ p))))

    (define-famous-formula material-implication-implicational-antecedent
	(-> (-> p q)
	    (v (¬ p) q)))

    (define-famous-formula material-implication-disjunctive-antecedent
	(-> (v (¬ p) q)
	    (-> p q)))

    (define-famous-formula material-equivalence-conjunctive-antecedent
	(-> (& (-> p q) (-> q p))
	    (v (& p q) (& (¬ p) (¬ q)))))

    (define-famous-formula material-equivalence-disjunctive-antecedent
	(-> (v (& p q) (& (¬ p) (¬ q)))
	    (& (-> p q) (-> q p))))

    (define-famous-formula exportation-conjunctive-antecedent
	(-> (-> (& p q) r)
	    (-> p (-> q r))))

    (define-famous-formula exportation-implicational-antecedent
	(-> (-> p (-> q r))
	    (-> (& p q) r)))

    ))

;;; famous-formulas.lisp ends here