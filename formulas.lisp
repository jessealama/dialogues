;;; formulas.lisp A representation for propositional and first-order formulas

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Keywords

(defclass formula ()
  nil)

(defclass atomic-formula (formula)
  ((predicate :initarg :predicate
	      :accessor predicate)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defun make-atomic-formula (predicate &rest arguments)
  (make-instance 'atomic-formula
		 :predicate predicate
		 :args arguments))

(defvar contradiction (make-atomic-formula '⊥))
(defvar top (make-atomic-formula '⊤))

(defun make-equation (lhs rhs)
  (make-atomic-formula '= lhs rhs))

(defclass composite-formula (formula)
  nil)

(defclass negation (formula)
  ((negated :initarg :negated
	    :accessor unnegate
	    :type formula)))

(defgeneric negate (formula))

(defmethod negate ((formula formula))
  (make-instance 'negation :negated formula))

(defclass binary-connective-formula (composite-formula)
  ((lhs :initarg :lhs
	:accessor lhs
	:type formula)
   (rhs :initarg :rhs
	:accessor rhs
	:type formula)))

(defclass multiple-arity-connective-formula (composite-formula)
  ((items :initarg :items
	  :accessor items
	  :type list)))

(defclass implication (binary-connective-formula)
  nil)

(defun make-implication (antecedent consequent)
  (make-instance 'implication
		 :lhs antecedent
		 :rhs consequent))

(defgeneric antecedent (formula))
(defgeneric consequent (formula))

(defmethod antecedent ((implication implication))
  (lhs implication))

(defmethod consequent ((implication implication))
  (rhs implication))

(defclass equivalence (binary-connective-formula)
  nil)

(defun make-equivalence (lhs rhs)
  (make-instance 'equivalence
		 :lhs lhs
		 :rhs rhs))

;;; disjunctions

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defclass multiple-arity-disjunction (multiple-arity-connective-formula)
  nil)

(defun make-binary-disjunction (lhs rhs)
  (make-instance 'binary-disjunction
		 :lhs lhs
		 :rhs rhs))

(defun make-multiple-arity-disjunction (&rest disjuncts)
  (if disjuncts
      (if (cdr disjuncts)
	  (if (cddr disjuncts)
	      (make-instance 'multiple-arity-disjunction
			     :items disjuncts)
	      (car disjuncts)))
      top))

(defun binary-disjunction->multiple-arity-disjunction (binary-disjunction)
  (make-instance 'multiple-arity-disjunction
		 :items (list (lhs binary-disjunction)
			      (rhs binary-disjunction))))

(defun multiple-arity-disjunction->binary-disjunction (multiple-arity-disjunction)
  (let ((disjuncts (items multiple-arity-disjunction)))
    (if (null disjuncts)
	(make-instance 'binary-disjunction
		       :lhs top
		       :rhs top)
	(if (null (cdr disjuncts))
	    (make-instance 'binary-disjunction
			   :lhs (first disjuncts)
			   :rhs contradiction)
	    (labels ((make-disjunction (ds)
		       (if (null (cddr ds))
			   (make-binary-disjunction (first ds)
						    (second ds))
			   (make-binary-disjunction (first ds)
						    (make-disjunction (cdr ds))))))
	      (make-disjunction disjuncts))))))

;; conjunctions

(defclass binary-conjunction (binary-connective-formula)
  nil)

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defun make-binary-conjunction (lhs rhs)
  (make-instance 'binary-conjunction
		 :lhs lhs
		 :rhs rhs))

(defun make-multiple-arity-conjunction (&rest conjuncts)
  (if conjuncts
      (if (cdr conjuncts)
	  (if (cddr conjuncts)
	      (make-instance 'multiple-arity-conjunction
			     :items conjuncts))
	  (cadr conjuncts))
      contradiction))

(defun binary-conjunction->multiple-arity-conjunction (binary-conjunction)
  (make-instance 'multiple-arity-conjunction
		 :items (list (lhs binary-conjunction)
			      (rhs binary-conjunction))))

(defun multiple-arity-conjunction->binary-conjunction (multiple-arity-conjunction)
  (let ((conjuncts (items multiple-arity-conjunction)))
    (if (null conjuncts)
	(make-binary-conjunction contradiction contradiction)
	(if (null (cdr conjuncts))
	    (make-instance 'binary-conjunction
			   :lhs (first conjuncts)
			   :rhs top)
	    (labels ((make-conjunction (ds)
		       (if (null (cddr ds))
			   (make-binary-conjunction (first ds) 
						    (second ds))
			   (make-binary-conjunction (first ds)
						    (make-conjunction (cdr ds))))))
	      (make-conjunction conjuncts))))))

;; quantifiers

(defclass generalization (composite-formula)
  ((bound-variable :initarg :bound-variable
		   :accessor bound-variable
		   :type variable-term)
   (matrix :initarg :matrix
	   :accessor matrix
	   :type formula)))

(defclass universal-generalization (generalization)
  nil)

(defclass existential-generalization (generalization)
  nil)

(defun make-universal (var formula)
  (make-instance 'universal-generalization
		 :bound-variable var
		 :matrix formula))

(defun make-existential (var formula)
  (make-instance 'existential-generalization
		 :bound-variable var
		 :matrix formula))

(defun equal-formulas? (form-1 form-2 signature)
  "Determine whether formulas FORM-1 and FORM-2 are equal."
  (declare (ignore signature)) ;; A bit unsatisfactory, but it'll do for now
  (equalp form-1 form-2))

(defun equation? (formula)
  (eq (car formula) '=))

(defun account-for-extension (constants predicate)
  "Make a formula saying that the extension of PREDICATE is exhausted
by the list CONSTANTS of constant symbols.  E.g, 

\(ACCOUNT-FOR-EXTENSION '(A B C) 'VERTEX\)

should return the formula

\(ALL ?X (IMPLIES (VERTEX ?X) (OR (= ?X A) (= ?X B) (= ?X C)))\)"
  (let ((var (make-variable "x")))
    (make-universal var
		    (make-implication
		     (make-atomic-formula predicate var)
		     (apply #'make-multiple-arity-disjunction
			    (mapcar #'(lambda (constant)
					(make-equation var constant))
				    constants))))))

(defgeneric proper-subformulas-1 (formula))

(defmethod proper-subformulas-1 ((formula atomic-formula))
  nil)

(defmethod proper-subformulas-1 ((negation negation))
  (let ((inside (unnegate negation)))
    (cons inside (proper-subformulas-1 inside))))

(defmethod proper-subformulas-1 ((formula binary-connective-formula))
  (let ((lhs (lhs formula))
	(rhs (rhs formula)))
    (append (list lhs rhs)
	    (proper-subformulas-1 lhs)
	    (proper-subformulas-1 rhs))))

(defmethod proper-subformulas-1 ((formula multiple-arity-connective-formula))
  (let ((items (items formula)))
    (append items
	    (mapcar #'proper-subformulas-1 items))))

(defmethod proper-subformulas-1 ((formula generalization))
  (let ((matrix (matrix formula)))
    (cons matrix (proper-subformulas-1 matrix))))

(defun proper-subformulas (formula)
  (remove-duplicates (proper-subformulas-1 formula) :test #'equal-formulas?))

(define-condition incompatible-variable-kinds-error (error)
  ()
  (:report (lambda (condition stream)
	     (format stream
		     "One cannot treat one kind of variable (sorted or unsorted) as the other kind"))))

(defgeneric subst-term-for-var-in-term (term var target-term))

(defmethod subst-term-for-var-in-term ((term term)
				       (var unsorted-variable)
				       (target-term unsorted-variable))
  (if (equal-variables? var target-term)
      term
      target-term))

(defmethod subst-term-for-var-in-term ((term term)
				       (var unsorted-variable)
				       (target-term sorted-variable))
  (error 'incompatible-variable-kinds-error))

(defmethod subst-term-for-var-in-term ((term term)
				       (var sorted-variable)
				       (target-term unsorted-variable))
  (error 'incompatible-variable-kinds-error))	 

(defmethod subst-term-for-var-in-term ((term term)
				       (var sorted-variable)
				       (target-term sorted-variable))
  (if (equal-variables? var target-term)
      term
      target-term))

(defmethod subst-term-for-var-in-term ((term term)
				       (var unsorted-variable)
				       (taget-term function-term))
  (let ((f (function-symbol target-term))
	(args (arguments target-term)))
    (apply #'make-function-term 
	   f
	   (mapcar #'(lambda (x) (subst-term-for-var-in-term term variable x))
		   args))))

(defgeneric instantiate (term variable formula)
  (:documentation "Substitute TERM for free occurances of VARIBLE in FORMULA.

WARNING: No regard is given to variables appearing in TERM that may become
bound once the substitution is carried out: no renaming is done either
in TERM or FORMULA."))  

(defmethod instantiate (term variable (formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (apply #'make-atomic-formula
	   pred
	   (mapcar #'(lambda (arg) 
		       (subst-term-for-var-in-term term variable arg)) 
		   args))))

(defmethod instantiate (term variable (formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (instantiate term variable (lhs formula))
		 :rhs (instantiate term variable (rhs formula))))

(defmethod instantiate (term variable (formula multiple-arity-connective-formula))
  (make-instance (class-of formula)
		 :items (mapcar #'(lambda (item)
				    (instantiate term variable item))
				(items formula))))

(defmethod instantiate (term variable (formula generalization))
  (let ((bound-var (bound-variable formula))
	(matrix (matrix formula)))
    (if (equal-variables? bound-var variable)
	formula
	(make-instance (class-of formula)
		       :bound-var bound-var
		       :matrix (instantiate term variable matrix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unification of formulas and terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unify (formula-or-term-1 formula-or-term-2))

(defmethod unify ((formula formula) (term term))
  nil)

(defmethod unify ((term term) (formula formula))
  nil)

(defmethod unify ((var-1 unsorted-variable) (var-2 unsorted-variable))
  (cons var-1 var-2))

(defmethod unify ((var-1 unsorted-variable) (var-2 sorted-variable))
  (error 'incompatible-variable-kinds-error))

(defmethod unify ((var-1 sorted-variable) (var-2 unsorted-variable))
  (error 'incompatible-variable-kinds-error))  

(defmethod unify ((var-1 sorted-variable) (var-2 sorted-variable))
  (let ((sort-1 (variable-sort var-1))
	(sort-2 (variable-sort var-2)))
    (if (eql sort-1 sort-2)
	(cons var-1 var-2)
	:fail)))

(defmethod unify ((var variable-term) (term function-term))
  (if (occurs-in-term? var (arguments term))
      :fail
      (cons var term)))

(defmethod unify ((term function-term) (var variable-term))
  (if (occurs-in-term? var (arguments term))
      :fail
      (cons var term)))

(defmethod unify ((term-1 function-term) (term-2 function-term))
  (let ((func-1 (function-symbol term-1))
	(args-1 (arguments term-1))
	(func-2 (function-symbol term-2))
	(args-2 (arguments term-2)))
    (if (eql func-1 func-2)
	(unify args-1 args-2)
	:fail)))

(defmethod unify ((list-1 list) (list-2 list))
  (if (null list-1)
      (if (null list-2)
	  nil
	  :fail)
      (if (null list-2)
	  :fail
	  (let ((first-1 (car list-1))
		(first-2 (car list-2)))
	    (let ((mgu-head (unify first-1 first-2)))
	      (if (eql mgu-head :fail)
		  :fail
		  (let ((new-tail-1 (apply-substitution mgu-head (cdr list-1)))
			(new-tail-2 (apply-substitution mgu-head (cdr list-2))))
		    (let ((mgu-tail (unify new-tail-1 new-tail-2)))
		      (if (eql mgu-tail :fail)
			  :fail
			  (compose-substitutions mgu-head mgu-tail))))))))))
	

(defun instance-of-quantified? (signature instantiated quantified-statement)
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
			     (instance-helper (lhs formula-1)
					      (rhs formula-2))))
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
				(equal-formulas? formula-2 instance-matrix signature)
				(and (universal? formula-2)
				     (instance-helper instance-matrix formula-2))))))
		       ((existential? formula-1)
			(when (existential? formula-2)
			  (let ((instance-var (bound-variable formula-1))
				(instance-matrix (matrix formula-1)))
			    (if (equal-variables? bound-variable instance-var)
				(equal-formulas? formula-2 instance-matrix signature)
				(and (universal? formula-2)
				     (instance-helper instance-matrix formula-2))))))
		       (t ;; atomic case
			(and (atomic-formula? signature formula-2)
			   (eq (predicate formula-1)
			       (predicate formula-2))
			   (every-pair #'(lambda (term-1 term-2)
					   (if (variable? term-2)
					       (if (equal-variables? term-2 bound-variable)
						   (if instance-term
						       (equal-terms? term-1 instance-term signature)
						       (setf instance-term term-1))
						   (and (variable? term-1)
							(equal-variables? term-1 term-2)))
					       (equal-terms? term-1 term-2 signature)))
				       (arguments formula-1)
				       (arguments formula-2)))))))
	(values (instance-helper instantiated matrix)
		instance-term)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sundry formula-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contains-formula? (lst formula)
  (member formula lst :test #'equal-formulas?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition malformed-formula-error (error)
  ((text :initarg :text 
	 :reader malformed-formula-error-text)
   (signature :initarg :signature 
	      :reader malformed-formula-error-signature))
  (:report (lambda (condition stream)
	     (let ((text (malformed-formula-error-text condition))
		   (sig (malformed-formula-error-signature condition)))
	       (if sig
		   (if text
		       (format stream 
			       "The given text,~%~%  ~A,~%~%is not a formula according to the signature~%~%  ~A~%"
			       text sig)
		       (format stream
			       "Although a signature,~%~%  ~A~%~%was supplied, no text was given.~%"
			       sig))
		   (if text
		       (format stream
			       "Unable to determine whether the given text,~%~%  ~A,~%~%is a formula because no signature was supplied.~%"
			       text)
		       (format stream
			       "Neither a signature nor a text were supplied.~%")))))))

(defun try-another-formula (c)
  (declare (ignore c))
  (let ((restart (find-restart 'try-another-formula)))
    (when restart
      (invoke-restart 'try-another-formula))))

(defun read-formula (&optional (stream t) 
		               (signature pqrs-propositional-signature))
  (let ((input (read stream nil nil)))
    (if (formula? input signature)
	input
	(error 'malformed-formula-error :text input
	                                :signature signature))))

(defun parse-formula (str &optional (signature pqrs-propositional-signature))
  (with-input-from-string (s str)
    (read-formula s signature)))

(defun read-new-formula ()
  (format t "Enter a new formula: ")
  (multiple-value-list (read-formula)))

;; The original READ-FORMULA.
;;
;; This function combines reading input and validating it.  It seems
;; more elegant, and more flexible, for this function to signal an
;; error when the input is not a formula (according to the signature).
;; The function then does just one thing: read a formula.  If the
;; input is not well-formed by its lights, it signals a condition; it
;; does not adopt its own policy for what to do when its input is
;; malformed.  Other, higher-level code can set a policy for what to
;; do when READ-FORMULA is asked to process malformed input.  (One
;; policy would be to prompt the user to enter new input until a vaid
;; formula is read, as this function now does.  Another would be to
;; really signal an error if the input is malformed.  Perhaps another
;; policy would be to use a specific formula.)
;;
;; (defun read-formula (signature)
;;   (let (response)
;;     (until (formula? response signature)
;;       (setf response (read t nil nil)))
;;     response))

(defun read-atomic-formula (signature)
  (let (response)
    (until (atomic-formula? signature response)
      (setf response (read t nil nil)))
    response))

(define-condition non-atomic-formula-error (error)
  ((text :initarg :text 
	 :reader non-atomic-formula-error-text)
   (signature :initarg :signature
	      :reader non-atomic-formula-error-signature))
  (:report (lambda (condition stream)
	     (let ((text (non-atomic-formula-error-text condition))
		   (sig (non-atomic-formula-error-signature condition)))
	       (if text
		   (if sig
		       (format stream 
			       "The given text,~%~%  ~A,~%~%is not a non-atomic formula according to the signature~%~%  ~A.~%"
			       text
			       sig)
		       (format stream
			       "Unable to determine whether the given text,~%~%  ~A,~%~%is a non-atomic formula because no signature was supplied.~%"
			       text))
		   (if sig
		       (format stream
			       "No text was given, though a signature~%~%~A~%~%was.~%"
			       sig)
		       (format stream
			       "Neither a text nor a signature was given.~%")))))))

(defun read-composite-formula (&optional (stream t)
			                 (signature pqrs-propositional-signature))
  (let ((input (read stream nil nil)))
    (if (formula? input signature)
	(if (atomic-formula? input signature)
	    (error 'non-atomic-formula-error :text input
		                             :signature signature)
	    input)
	(error 'malformed-formula-error :text input
   	                                :signature signature))))
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Named formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter peirce-formula (make-implication 
			      (make-implication
			       (make-implication 'p 'q)
			       'p)
			      'p))

(defparameter excluded-middle (make-disjunction 'p (negate 'p)))

(defparameter dummett-formula (make-disjunction (make-implication 'p 'q)
						(make-implication 'q 'p)))

(defparameter markov-formula (make-implication (negate (negate 'p))
					       'p))

(defparameter double-negation-intro (make-implication 'p
						      (negate (negate 'p))))

(defparameter double-negation-elimination (make-implication (negate (negate 'p))
							    'p))

(defparameter k-formula (make-implication 'p
					  (make-implication 'q 'p)))

(defparameter b-formula (make-implication
			 (make-implication 'p 'q)
			 (make-implication
			  (make-implication 'r 'p)
			  (make-implication 'r 'q))))

(defparameter c-formula (make-implication
			 (make-implication 'p
					   (make-implication 'q 'r))
			 (make-implication 'q
					   (make-implication 'p 'r))))

(defparameter w-formula (make-implication
			 (make-implication 'p
					   (make-implication 'p 'q))
			 (make-implication 'p 'q)))

(defparameter weak-excluded-middle (make-disjunction
				    (negate 'p)
				    (negate (negate 'p))))

(defparameter scott-formula (make-implication (make-implication double-negation-elimination
								excluded-middle)
					      weak-excluded-middle))

(defparameter smetanich-formula (make-implication (make-implication (negate 'q) 'p)
						  peirce-formula))

;;; formulas.lisp ends here