;;; translations.lisp Some formula translations

(in-package :dialogues)

(defgeneric gödel-gentzen (formula))

(defmethod gödel-gentzen ((formula atomic-formula))
  (negate (negate formula)))

(defmethod gödel-gentzen ((negation negation))
  (negate (gödel-gentzen (unnegate negation))))

(defmethod gödel-gentzen ((conjunction binary-conjunction))
  (make-binary-conjunction (gödel-gentzen (lhs conjunction))
			   (gödel-gentzen (rhs conjunction))))

(defmethod gödel-gentzen ((disjunction binary-disjunction))
  (negate (make-binary-disjunction (negate (gödel-gentzen (lhs disjunction)))
				   (negate (gödel-gentzen (rhs disjunction))))))

(defmethod gödel-gentzen ((implication implication))
  (make-implication (gödel-gentzen (antecedent implication))
		    (gödel-gentzen (consequent implication))))

(defmethod gödel-gentzen ((universal universal-generalization))
  (make-universal (bound-variable universal)
		  (gödel-gentzen (matrix universal))))

(defmethod gödel-gentzen ((existential existential-generalization))
  (negate (make-universal (bound-variable existential)
			  (negate (gödel-gentzen (matrix existential))))))

;;; translations.lisp ends here