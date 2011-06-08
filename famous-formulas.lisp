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
	(v p (neg p)))
    
    (define-famous-formula dummett-formula 
	(v (-> p q) (-> q p)))
    
    (define-famous-formula double-negation-introduction
	(-> p
	    (neg (neg p))))
    
    (define-famous-formula double-negation-elimination
	(-> (neg (neg p)) p))
    
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
	 (neg p)
	 (neg (neg p))))
    
    (define-famous-formula scott-formula 
	(-> (-> (-> (neg (neg p)) p) (v p (neg p)))
	    (v (neg p) (neg (neg p)))))

    (define-famous-formula smetanich-formula 
	(-> (-> (neg q) p) 
	    (-> (-> (-> p q) p) p)))
    
    (define-famous-formula de-morgan-not-and-implies-or-not
	(-> (neg (& p q))
	    (v (neg p) (neg q))))

    (define-famous-formula de-morgan-not-or-implies-and-not
	(-> (neg (v p q))
	    (& (neg p) (neg q))))

    (define-famous-formula de-morgan-and-not-implies-not-or
	(-> (& (neg p) (neg q))
	    (neg (v p q))))

    (define-famous-formula de-morgan-or-not-implies-not-and
	(-> (v (neg p) (neg q))
	    (neg (& p q))))

    (define-famous-formula anti-connexive-formula
	(v (-> p (neg p))
	   (-> (neg p) p)))

    (define-famous-formula ex-contradictione-quodlibet
	(-> (& p (neg p))
	    q))

    (define-famous-formula aristotles-thesis-positive-antecedent
	(neg (-> p (neg p))))

    (define-famous-formula aristotles-thesis-negative-antecedent
	(neg (-> (neg p) p)))

    (define-famous-formula conditional-excluded-middle
	(v (-> p q) (-> p (neg q))))

    (define-famous-formula implicational-ex-falso
	(-> (neg p) (-> p q)))

    (define-famous-formula wkp
	(-> (-> (neg p)
		(v (neg q) (neg r)))
	    (v (-> (neg p) (neg q))
	       (-> (neg p) (neg r)))))

    (define-famous-formula modus-ponens
	(-> (& (-> p q) p) q))
    
    (define-famous-formula modus-tollens
	(-> (& (-> p q) (neg q)) (neg p)))

    (define-famous-formula hypothetical-syllogism
	(-> (& (-> p q) (-> q r))
	    (-> p r)))

    (define-famous-formula disjunctive-syllogism
	(-> (& (v p q) (neg p)) (neg q)))

    (define-famous-formula constructive-dilemma
	(-> (& (-> p q)
	       (-> r s))
	    (-> (v p r)
		(v q s))))

    (define-famous-formula destructive-dilemma
	(-> (& (-> p q)
	       (-> r s))
	    (-> (v (neg r) (neg s))
		(v (neg p) (neg q)))))

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
	(-> (& p (v q r)) (v (& p q) (& p r))))

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
	(-> (-> p q) (-> (neg q) (neg p))))

    (define-famous-formula material-implication-implicational-antecedent
	(-> (-> p q)
	    (v (neg p) q)))

    (define-famous-formula material-implication-disjunctive-antecedent
	(-> (v (neg p) q)
	    (-> p q)))

    (define-famous-formula material-implication-negative-antecedent
	(-> (-> p q) (neg (& p (neg q)))))

    (define-famous-formula material-implication-negative-consequent
	(-> (neg (& p (neg q))) (-> p q)))

    (define-famous-formula material-equivalence-conjunctive-antecedent
	(-> (& (-> p q) (-> q p))
	    (v (& p q) (& (neg p) (neg q)))))

    (define-famous-formula material-equivalence-disjunctive-antecedent
	(-> (v (& p q) (& (neg p) (neg q)))
	    (& (-> p q) (-> q p))))

    (define-famous-formula exportation-conjunctive-antecedent
	(-> (-> (& p q) r)
	    (-> p (-> q r))))

    (define-famous-formula exportation-implicational-antecedent
	(-> (-> p (-> q r))
	    (-> (& p q) r)))

    (define-famous-formula conjunctive-idempotency-conjunctive-antecedent
	(-> (& p p) p))

    (define-famous-formula conjunctive-idempotency-conjunctive-consequent
	(-> p (& p p)))

    (define-famous-formula disjunctive-idempotenency-disjunctive-antecedent
	(-> (v p p) p))

    (define-famous-formula disjunctive-idempotenency-disjunctive-consequent
	(-> p (v p p)))

    (define-famous-formula disjunctive-absorption-disjunctive-antecedent
	(-> (v p (& p q)) p))

    (define-famous-formula disjunctive-absorption-disjunctive-consequent
	(converse disjunctive-absorption-disjunctive-antecedent))

    (define-famous-formula conjunctive-absorption-conjunctive-antecedent
	(-> (& p (v p q)) p))

    (define-famous-formula conjunctive-absorption-conjunctive-consequent
	(converse conjunctive-absorption-conjunctive-antecedent))

    (define-famous-formula frege-formula
	(-> (-> p (-> q r))
	    (-> (-> p q)
		(-> p r))))

    (define-famous-formula contraposition-positive-antecedent
	(-> (-> p q)
	    (-> (neg q) (neg p))))

    (define-famous-formula contraposition-negative-antecedent
	(-> (-> (neg q) (neg p))
	    (-> p q)))

    (define-famous-formula connexive-ax-1
	(-> (-> p q) (-> (-> q r) (-> p r))))

    (define-famous-formula connexive-ax-2
	(-> (-> (-> p p) q) q))

    (define-famous-formula connexive-ax-3
	(-> (-> p q) (-> (& p r) (& q r))))

    (define-famous-formula connexive-ax-4
	(-> (& q q) (-> p p)))

    (define-famous-formula connexive-ax-5
	(-> (& p (& q r)) (& q (& p r))))

    (define-famous-formula connexive-ax-6
	(-> (& p p) (-> (-> p p) (& p p))))

    (define-famous-formula connexive-ax-7
	(-> p (& p (& p p))))

    (define-famous-formula connexive-ax-8
	(-> (& (-> p (neg q)) q) (neg p)))

    (define-famous-formula connexive-ax-9
	(-> (& p (neg (& p (neg q)))) q))

    (define-famous-formula connexive-ax-10
	(neg (& p (neg (& p p)))))

    (define-famous-formula connexive-ax-11
	(v (v (neg p) (-> (-> p p) p))
	   (-> (v (-> p p) (-> p p)) p)))

    (define-famous-formula connexive-ax-12
	(-> (-> p p) (neg (-> p (neg p)))))

    ))

(defparameter il-axioms
  (let ((p (make-atomic-formula 'p))
	(q (make-atomic-formula 'q))
	(r (make-atomic-formula 'r)))
    (list (-> p (-> q p))
	  (-> (-> p q) (-> (-> p (-> q r)) (-> p r)))
	  (-> p (-> q (& p q)))
	  (-> (& p q) p)
	  (-> (& p q) q)
	  (-> p (v p q))
	  (-> q (v p q))
	  (-> (-> p r) (-> (-> q r) (-> (v p q) r)))
	  (-> (-> p q) (-> (-> p (neg q)) (neg p)))
	  (-> (neg p) (-> p q)))))

;;; famous-formulas.lisp ends here