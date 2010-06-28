;;; translations.lisp Some formula translations

(in-package :dialogues)

;;; Gödel-Gentzen translation

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

;;; Simple double negation

(defun double-negate (formula)
  (negate (negate formula)))

;;; All-subformulas translation

(defgeneric dn-all-subformulas (formula))

(defmethod dn-all-subformulas ((formula atomic-formula))
  (negate (negate formula)))

(defmethod dn-all-subformulas ((formula unary-connective-formula))
  (negate (negate (dn-all-subformulas (argument formula)))))

(defmethod dn-all-subformulas ((formula binary-connective-formula))
  (negate (negate (make-instance (class-of formula)
				 :lhs (dn-all-subformulas (lhs formula))
				 :rhs (dn-all-subformulas (rhs formula))))))

(defmethod dn-all-subformulas ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (negate (negate (dn-all-subformulas (matrix gen))))))

;;; Kuroda translation

(defun kuroda (formula)
  (labels ((kuroda-helper (form)
	     (cond ((atomic-formula? form) form)
		   ((negation? form)
		    (negate (kuroda-helper (unnegate form))))
		   ((binary-connective-formula? form)
		    (make-instance (class-of form)
				   :lhs (kuroda-helper (lhs form))
				   :rhs (kuroda-helper (rhs form))))
		   ((existential-generalization? form)
		    (make-existential (bound-variable form)
				      (kuroda-helper (matrix form))))
		   ((universal-generalization? form)
		    (make-universal (bound-variable form)
				    (negate 
				     (negate
				      (kuroda-helper (matrix form)))))))))
    (negate (negate (kuroda-helper formula)))))


;;; translations.lisp ends here