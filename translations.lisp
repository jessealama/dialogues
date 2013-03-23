;;; translations.lisp Some formula translations

(in-package :dialogues)

(defclass formula-translation ()
  ((transformer :type function
		:initarg :transformer
		:initform #'identity
		:accessor transformer)
   (description :type string
		:initarg :description
		:initform "(no description was supplied)"
		:accessor description)))

(defun apply-translation (translation formula)
  (funcall (transformer translation) formula))

;;; Gödel-Gentzen translation

(defgeneric goedel-gentzen (formula)
  (:documentation "The Goedel-Gentzen negative translation."))

(defmethod goedel-gentzen ((formula atomic-formula))
  (negate (negate formula)))

(defmethod goedel-gentzen ((negation negation))
  (negate (goedel-gentzen (unnegate negation))))

(defmethod goedel-gentzen ((conjunction binary-conjunction))
  (make-binary-conjunction (goedel-gentzen (lhs conjunction))
			   (goedel-gentzen (rhs conjunction))))

(defmethod goedel-gentzen ((disjunction binary-disjunction))
  (negate (make-binary-conjunction (negate (goedel-gentzen (lhs disjunction)))
				   (negate (goedel-gentzen (rhs disjunction))))))

(defmethod goedel-gentzen ((implication implication))
  (make-implication (goedel-gentzen (antecedent implication))
		    (goedel-gentzen (consequent implication))))

(defmethod goedel-gentzen ((universal universal-generalization))
  (make-universal (bound-variable universal)
		  (goedel-gentzen (matrix universal))))

(defmethod goedel-gentzen ((existential existential-generalization))
  (negate (make-universal (bound-variable existential)
			  (negate (goedel-gentzen (matrix existential))))))

(defparameter goedel-gentzen-translation
  (make-instance 'formula-translation
		 :description "The Goedel-Gentzen negative translation"
		 :transformer #'goedel-gentzen))

;;; Simple double negation

(defun double-negate (formula)
  "Double negate the whole formula"
  (negate (negate formula)))

(defparameter double-negate-translation
  (make-instance 'formula-translation
		 :description "Double negate the whole formula"
		 :transformer #'double-negate))

;;; All-subformulas translation

(defgeneric dn-all-subformulas (formula)
  (:documentation "Double negate all subformulas"))

(defmethod dn-all-subformulas ((formula atomic-formula))
  (negate (negate formula)))

(defmethod dn-all-subformulas ((formula unary-connective-formula))
  (negate (negate (negate (dn-all-subformulas (argument formula))))))

(defmethod dn-all-subformulas ((formula binary-connective-formula))
  (negate (negate (make-instance (class-of formula)
				 :lhs (dn-all-subformulas (lhs formula))
				 :rhs (dn-all-subformulas (rhs formula))))))

(defmethod dn-all-subformulas ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (negate (negate (dn-all-subformulas (matrix gen))))))

(defparameter double-negate-all-subformulas-translation
  (make-instance 'formula-translation
		 :description "Double negate all subformulas"
		 :transformer #'dn-all-subformulas))

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

(defparameter kuroda-translation
  (make-instance 'formula-translation
		 :description "The Kuroda translation"
		 :transformer #'kuroda))

;;; Replace all atomic formulas by their negations

(defgeneric negate-atomic-subformulas (formula)
  (:documentation "Negate atomic subformulas"))

(defmethod negate-atomic-subformulas ((formula atomic-formula))
  (negate formula))

(defmethod negate-atomic-subformulas ((formula unary-connective-formula))
  (make-instance (class-of formula)
		 :argument (negate-atomic-subformulas (argument formula))))

(defmethod negate-atomic-subformulas ((formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (negate-atomic-subformulas (lhs formula))
		 :rhs (negate-atomic-subformulas (rhs formula))))

(defmethod negate-atomic-subformulas ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (negate-atomic-subformulas (matrix gen))))

(defparameter negate-atomic-subformulas-translation
  (make-instance 'formula-translation
		 :description "Negate atomic subformulas"
		 :transformer #'negate-atomic-subformulas))

;; Replace all atomic subformulas by their double negations

(defgeneric double-negate-atomic-subformulas (formula)
  (:documentation "Double negate atomic subformulas"))

(defmethod double-negate-atomic-subformulas ((formula atomic-formula))
  (negate (negate formula)))

(defmethod double-negate-atomic-subformulas ((formula unary-connective-formula))
  (make-instance (class-of formula)
		 :argument (double-negate-atomic-subformulas (argument formula))))

(defmethod double-negate-atomic-subformulas ((formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (double-negate-atomic-subformulas (lhs formula))
		 :rhs (double-negate-atomic-subformulas (rhs formula))))

(defmethod double-negate-atomic-subformulas ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (double-negate-atomic-subformulas (matrix gen))))

(defparameter double-negate-atomic-subformulas-translation
  (make-instance 'formula-translation
		 :description "Double negate atomic subformulas"
		 :transformer #'double-negate-atomic-subformulas))

;; Replace all atomic subformulas by their "self-conjunctions"

(defgeneric self-conjoin-atomic-subformulas (formula)
  (:documentation "Replace all atomic subformulas p by (p &and; p)"))

(defmethod self-conjoin-atomic-subformulas ((formula atomic-formula))
  (make-binary-conjunction formula formula))

(defmethod self-conjoin-atomic-subformulas ((formula unary-connective-formula))
  (make-instance (class-of formula)
		 :argument (self-conjoin-atomic-subformulas (argument formula))))

(defmethod self-conjoin-atomic-subformulas ((formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (self-conjoin-atomic-subformulas (lhs formula))
		 :rhs (self-conjoin-atomic-subformulas (rhs formula))))

(defmethod self-conjoin-atomic-subformulas ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (self-conjoin-atomic-subformulas (matrix gen))))

(defparameter self-conjoin-atomic-subformulas-translation
  (make-instance 'formula-translation
		 :description "Replace all atomic subformulas p by (p &and; p)"
		 :transformer #'self-conjoin-atomic-subformulas))

;; Replace all atomic subformulas by their "self-disjunctions"

(defgeneric self-disjoin-atomic-subformulas (formula)
  (:documentation "Replace all atomic subformulas p by (p &or; p)"))

(defmethod self-disjoin-atomic-subformulas ((formula atomic-formula))
  (make-binary-disjunction formula formula))

(defmethod self-disjoin-atomic-subformulas ((formula unary-connective-formula))
  (make-instance (class-of formula)
		 :argument (self-disjoin-atomic-subformulas (argument formula))))

(defmethod self-disjoin-atomic-subformulas ((formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (self-disjoin-atomic-subformulas (lhs formula))
		 :rhs (self-disjoin-atomic-subformulas (rhs formula))))

(defmethod self-disjoin-atomic-subformulas ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (self-disjoin-atomic-subformulas (matrix gen))))

(defparameter self-disjoin-atomic-subformulas-translation
  (make-instance 'formula-translation
		 :description "Replace all atomic subformulas p by (p &or; p)"
		 :transformer #'self-disjoin-atomic-subformulas))

;; Contrapositive of all implications

(defgeneric contrapositivify (formula)
  (:documentation "Take the contrapositive of all implications"))

(defmethod contrapositivify ((formula atomic-formula))
  formula)

(defmethod contrapositivify ((formula binary-conjunction))
  (make-binary-conjunction (contrapositivify (lhs formula))
			   (contrapositivify (rhs formula))))

(defmethod contrapositivify ((formula binary-disjunction))
  (make-binary-disjunction (contrapositivify (lhs formula))
			   (contrapositivify (rhs formula))))

(defmethod contrapositivify ((neg negation))
  (negate (contrapositivify (argument neg))))

(defmethod contrapositivify ((equiv equivalence))
  (make-equivalence (contrapositivify (lhs equiv))
		    (contrapositivify (rhs equiv))))

(defmethod contrapositivify ((imp implication))
  (make-implication (negate (contrapositivify (consequent imp)))
		    (negate (contrapositivify (antecedent imp)))))

(defmethod contrapositivify ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (contrapositivify (matrix gen))))

(defparameter contrapositivify-translation
  (make-instance 'formula-translation
		 :description "Take the contrapositive of all implications"
		 :transformer #'contrapositivify))

;; Contrapositive of an implication (but don't contrapositivify any
;; inner implications)

(defgeneric contrapositive (formula)
  (:documentation "Take the contrapositive of an implications (but do nothing inside the formula)"))

(defmethod contrapositive ((formula atomic-formula))
  formula)

(defmethod contrapositive ((formula binary-conjunction))
  formula)

(defmethod contrapositive ((formula binary-disjunction))
  formula)

(defmethod contrapositive ((neg negation))
  neg)

(defmethod contrapositive ((equiv equivalence))
  equiv)

(defmethod contrapositive ((imp implication))
  (make-implication (negate (consequent imp))
		    (negate (antecedent imp))))

(defmethod contrapositive ((gen generalization))
  gen)

(defparameter contrapositive-translation
  (make-instance 'formula-translation
		 :description "Take the contrapositive of all implications (but do nothing inside the formula)"
		 :transformer #'contrapositive))

;; Identity translation

(defparameter identity-translation
  (make-instance 'formula-translation
		 :description "Identity function"
		 :transformer #'identity))

;; atomics to instances of excluded middle

(defgeneric atomic->excluded-middle (formula))

(defmethod atomic->excluded-middle ((form atomic-formula))
  (make-binary-disjunction form
			   (negate form)))

(defmethod atomic->excluded-middle ((formula unary-connective-formula))
  (make-instance (class-of formula)
		 :argument (atomic->excluded-middle (argument formula))))

(defmethod atomic->excluded-middle ((formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (atomic->excluded-middle (lhs formula))
		 :rhs (atomic->excluded-middle (rhs formula))))

(defmethod atomic->excluded-middle ((gen generalization))
  (make-instance (class-of gen)
		 :bound-variable (bound-variable gen)
		 :matrix (atomic->excluded-middle (matrix gen))))

(defparameter atomic->excluded-middle-translation
  (make-instance 'formula-translation
		 :description "Replace all atomic subformulas p by (p &or; &not;p)"
		 :transformer #'atomic->excluded-middle))

;;; converse

(defun converse (formula)
  (if (implication? formula)
      (make-implication (consequent formula)
			(antecedent formula))
      formula))

(defparameter converse-translation
  (make-instance 'formula-translation
		 :description "Take the converse of an implication (but do nothing to non-implications)"
		 :transformer #'converse))


;;; translations.lisp ends here
