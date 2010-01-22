;;; formulas.lisp A representation for first-order formulas

(defconstant contradiction '⊥)
(defconstant top '⊤)

(defun top? (formula)
  (eq (car formula) top))

(defun bottom? (formula)
  (eq (car formula) contradiction))

(defun negation? (formula)
  (eq (car formula) 'not))

(defun unnegate (negation)
  (second negation))

(defun implication? (formula)
  (eq (car formula) 'implies))

(defun antecedent (implication)
  (cadr implication))

(defun consequent (implication)
  (caddr implication))

(defun make-implication (antecedent consequent)
  (list 'implies antecedent consequent))

(defun equivalence? (formula)
  (eq (car formula) 'iff))

(defun disjunction? (formula)
  (eq (car formula) 'or))

(defun disjuncts (disjunction)
  (cdr disjunction))

(defun conjunction? (formula)
  (eq (car formula) 'and))

(defun conjuncts (conjunction)
  (cdr conjunction))

(defun universal? (formula)
  (eq (car formula) 'all))

(defun existential? (formula)
  (eq (car formula) 'exists))

(defun matrix (quantified-formula)
  (caddr quantified-formula))

(defun bound-variable (quantified-formula)
  (cadr quantified-formula))

(defun bare-variable? (variable)
  (symbolp variable))

(defun typed-variable? (variable)
  (not (bare-variable? variable)))

(defun variable-type (typed-variable)
  (second typed-variable))

(defun variable-name (typed-variable)
  (first typed-variable))

(defun v-statement? (formula)
  (eq (car formula) 'V))

(defun e-statement? (formula)
  (eq (car formula) 'E))

(defun f-statement? (formula)
  (eq (car formula) 'F))

(defun equation? (formula)
  (eq (car formula) '=))

(defun unary-statement-argument (unary-statement)
  (cadr unary-statement))

(defun binary-statement-first-arg (binary-statement)
  (cadr binary-statement))

(defun binary-statement-second-arg (binary-statement)
  (caddr binary-statement))

(defun formula? (x)
  (or (symbolp x)
      (eq x contradiction)
      (eq x top)
      (and x
	   (listp x)
	   (cdr x)
	   (not (listp (car x))))))

(defun atomic-formula? (formula)
  (or (symbolp formula)
      (and (not (disjunction? formula))
	   (not (conjunction? formula))
	   (not (equivalence? formula))
	   (not (implication? formula))
	   (not (negation? formula))
	   (not (universal? formula))
	   (not (existential? formula)))))

(defun equal-formulas? (form-1 form-2)
  "Determine whether formulas FORM-1 and FORM-2 are equal."
  (equalp form-1 form-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (sequent 
	     (:print-function print-sequent)
	     (:constructor make-seq))
  (lhs nil :type list)
  (rhs nil :type list))

(defun print-sequent (seq stream depth)
  (declare (ignore depth))
  (let ((lhs (sequent-lhs seq))
	(rhs (sequent-rhs seq)))
    (let ((lhs-as-string (comma-separated-list lhs))
	  (rhs-as-string (comma-separated-list rhs)))
      (format stream "~A => ~A" lhs-as-string rhs-as-string))))

(defun make-sequent (lhs rhs)
  (if (formula? lhs)
      (if (formula? rhs)
	  (make-seq :lhs (list lhs)
		    :rhs (list rhs))
	  (make-seq :lhs (list lhs)
		    :rhs rhs))
      (if (formula? rhs)
	  (make-seq :lhs lhs
		    :rhs (list rhs))
	  (make-seq :lhs lhs
		    :rhs rhs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sundry formula-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contains-formula? (lst formula)
  (member formula lst :test #'equal-formulas?))


(provide 'formulas)

;;; formulas.lisp ends here