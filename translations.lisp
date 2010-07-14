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

(defgeneric gödel-gentzen (formula)
  (:documentation "The Gödel-Gentzen negative translation."))

(defmethod gödel-gentzen ((formula atomic-formula))
  (negate (negate formula)))

(defmethod gödel-gentzen ((negation negation))
  (negate (gödel-gentzen (unnegate negation))))

(defmethod gödel-gentzen ((conjunction binary-conjunction))
  (make-binary-conjunction (gödel-gentzen (lhs conjunction))
			   (gödel-gentzen (rhs conjunction))))

(defmethod gödel-gentzen ((disjunction binary-disjunction))
  (negate (make-binary-conjunction (negate (gödel-gentzen (lhs disjunction)))
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

(defparameter gödel-gentzen-translation
  (make-instance 'formula-translation
		 :description "The Gödel-Gentzen negative translation"
		 :transformer #'gödel-gentzen))

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


;;; translations.lisp ends here