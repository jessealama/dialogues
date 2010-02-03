;;; formulas.lisp A representation for first-order formulas

(require 'utils "utils.lisp")

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

(defun negate (formula)
  (list 'not formula))

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

(defun make-disjunction (&rest disjuncts)
  (cons 'or disjuncts))

(defun left-disjunct (disjunction)
  (cadr disjunction))

(defun right-disjunct (disjunction)
  (caddr disjunction))

(defun conjunction? (formula)
  (eq (car formula) 'and))

(defun conjuncts (conjunction)
  (cdr conjunction))

(defun left-conjunct (conjunction)
  (cadr conjunction))

(defun right-conjunct (conjunction)
  (caddr conjunction))

(defun make-conjunction (&rest conjuncts)
  (cons 'and conjuncts))

(defun universal? (formula)
  (eq (car formula) 'all))

(defun make-universal (var formula)
  (list 'all var formula))

(defun existential? (formula)
  (eq (car formula) 'exists))

(defun make-existential (var formula)
  (list 'exists var formula))

(defun matrix (quantified-formula)
  (caddr quantified-formula))

(defun bound-variable (quantified-formula)
  (cadr quantified-formula))

(defun equal-variables? (var-1 var-2)
  (eql var-1 var-2))

(defun variable? (term)
  (and (symbolp term)
       (eq (aref (symbol-name term) 0) #\?)))

(defun term? (x)
  (or (variable? x)
      (symbolp? x)
      (and (listp x)
	   (not (null x))
	   (symbolp (car x))
	   (every #'term? (cdr x)))))

(defun function-symbol (complex-term)
  (car complex-term))

(defun term-arguments (complex-term)
  (cdr complex-term))

(defun make-complex-term (function &rest args)
  (cons function args))

(defun equal-terms? (term-1 term-2)
  (equalp term-1 term-2))

(defun subst-term-for-var-in-term (term variable target-term)
  (if (variable? target-term)
      (if (equal-variables? variable target-term)
	  term
	  target-term)
      (let ((f (function-symbol target-term))
	    (args (term-arguments target-term)))
	(apply #'make-complex-term f
	                           (mapcar #'(lambda (x) (subst-term-for-var-in-term term variable x))
					   args)))))    

(defun predicate (atomic-formula)
  (car atomic-formula))

(defun arguments (atomic-formula)
  (cdr atomic-formula))

(defun make-atomic-formula (predicate &rest arguments)
  (cons predicate arguments))

(defun equal-formulas? (form-1 form-2)
  "Determine whether formulas FORM-1 and FORM-2 are equal."
  (equalp form-1 form-2))

(defun instantiate (term variable formula)
  "Substitute TERM for free occurances of VARIBLE in FORMULA.

WARNING: No regard is given to variables appearing in TERM that may become
bound once the substitution is carried out: no renaming is done either
in TERM or FORMULA."
  (cond ((disjunction? formula)
	 (apply #'make-disjunction (mapcar #'(lambda (disjunct) (instantiate term variable disjunct))
					   (disjuncts formula))))
	((conjunction? formula)
	 (apply #'make-conjunction (mapcar #'(lambda (conjunct) (instantiate term variable conjunct))
					   (conjuncts formula))))
	((negation? formula)
	 (negate (instantiate term variable (unnegate formula))))
	((implication? formula)
	 (make-implication (instantiate term variable (antecedent formula))
			   (instantiate term variable (consequent formula))))
	((universal? formula)
	 (let ((bound-var (bound-variable formula))
	       (matrix (matrix formula)))
	   (if (equal bound-var variable)
	       formula
	       (make-universal bound-var (instantiate term variable matrix)))))
	((existential? formula)
	 (let ((bound-var (bound-variable formula))
	       (matrix (matrix formula)))
	   (if (equal bound-var variable)
	       formula
	       (make-existential bound-var (instantiate term variable matrix)))))
	(t ;; atomic case
	 (let ((predicate (predicate formula))
	       (arguments (arguments formula)))
	   (apply #'make-atomic-formula predicate
  		                        (mapcar #'(lambda (arg) (subst-term-for-var-in-term term variable arg)) arguments))))))

(defun instance-of-quantified? (instantiated quantified-statement)
  "Determine whether INSTANTIATED is obtained from
  QUANTIFIED-STATEMENT, (ALL ?X A) or (EXISTS ?X A), by plugging in a
  term for the free occurences of ?X."
  (when (or (universal? quantified-statement)
	    (existential? quantified-statement))
    (let ((bound-variable (bound-variable quantified-statement))
	  (matrix (matrix quantified-statement))
	  (instance-term nil))
      (labels ((instance-helper (formula-1 formula-2)
		 (cond ((conjunction? formula-1)
			(and (conjunction? formula-2)
			     (instance-helper (left-conjunct formula-1)
					      (left-conjunct formula-2))
			     (instance-helper (right-conjunct formula-1)
					      (right-conjunct formula-2))))
		       ((disjunction? formula-1)
			(and (disjunction? formula-2)
			     (instance-helper (left-disjunct formula-1)
					      (left-disjunct formula-2))))
		       ((implication? formula-1)
			(and (implication? formula-2)
			     (instance-helper (antecedent formula-1)
					      (antecedent formula-2))))
		       ((negation? formula-1)
			(and (negation? formula-2)
			     (instance-helper (unnegate formula-1)
					      (unnegate formula-2))))
		       ((universal? formula-1)
			(when (universal? formula-2)
			  (let ((instance-var (bound-variable formula-1))
				(instance-matrix (matrix formula-1)))
			    (if (equal-variables? bound-variable instance-var)
				(equal-formulas? formula-2 instance-matrix)
				(and (universal? formula-2)
				     (instance-helper instance-matrix formula-2))))))
		       ((existential? formula-1)
			(when (existential? formula-2)
			  (let ((instance-var (bound-variable formula-1))
				(instance-matrix (matrix formula-1)))
			    (if (equal-variables? bound-variable instance-var)
				(equal-formulas? formula-2 instance-matrix)
				(and (universal? formula-2)
				     (instance-helper instance-matrix formula-2))))))
		       (t ;; atomic case
			(and (atomic-formula? formula-2)
			   (eq (predicate formula-1)
			       (predicate formula-2))
			   (every-pair #'(lambda (term-1 term-2)
					   (if (variable? term-2)
					       (if (equal-variables? term-2 bound-variable)
						   (if instance-term
						       (equal-terms? term-1 instance-term)
						       (setf instance-term term-1))
						   (and (variable? term-1)
							(equal-variables? term-1 term-2)))
					       (equal-terms? term-1 term-2)))
				       (arguments formula-1)
				       (arguments formula-2)))))))
	(values (instance-helper instantiated matrix)
		instance-term)))))

(defun bare-variable? (variable)
  (symbolp variable))

(defun typed-variable? (variable)
  (not (bare-variable? variable)))

(defun variable-type (typed-variable)
  (second typed-variable))

(defun variable-name (typed-variable)
  (first typed-variable))

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

(defun read-term ()
  (let (response)
    (until (symbolp response)
      (setq response (read nil t t)))
    response))

(defun read-formula ()
  (let (response)
    (until (formula? response)
      (setf response (read nil t t)))
    response))

(defun read-atomic-formula ()
  (let (response)
    (until (atomic-formula? response)
      (setf response (read nil t t)))
    response))

(provide 'formulas)

;;; formulas.lisp ends here