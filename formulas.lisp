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

(defclass implication (composite-formula)
  ((antecedent :initarg :antecedent
	       :accessor antecedent
	       :type formula)
   (consequent :initarg :consequent
	       :accessor consequent
	       :type formula)))

(defun make-implication (antecedent consequent)
  (make-instance 'implication
		 :antecedent antecedent
		 :consequent consequent))

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

;; quantifiers

(defclass generalization (composite-formula)
  ((bound-variable :initarg :bound-variable
		   :accessor bound-variable
		   :type variable)
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
		     (make-atomic-statement predicate var)
		     (apply #'make-disjunction
			    (mapcar #'(lambda (constant)
					(make-equation var constant))
				    constants))))))

(defun proper-subformulas (formula)
  (labels 
      ((proper (f)
	 (cond ((disjunction? f)
		(let ((left (lhs f))
		      (right (rhs f)))
		  (append (list left right)
			  (proper-subformulas left)
			  (proper-subformulas right))))
	       ((conjunction? f)
		(let ((left (left-conjunct f))
		      (right (right-conjunct f)))
		  (append (list left right)
			  (proper-subformulas left)
			  (proper-subformulas right))))
	       ((implication? f)
		(let ((a (antecedent f))
		      (c (consequent f)))
		  (append (list a c)
			  (proper-subformulas a)
			  (proper-subformulas c))))
	       ((equivalence? f)
		(let ((lhs (lhs f))
		      (rhs (rhs f)))
		  (append (list lhs rhs)
			  (proper-subformulas lhs)
			  (proper-subformulas rhs))))
	       ((negation? f)
		(cons (unnegate f)
		      (proper-subformulas (unnegate f))))
	       ((universal? f)
		(cons (matrix f)
		      (proper-subformulas (matrix f))))
	       ((existential? f)
		(cons (matrix f)
		      (proper-subformulas (matrix f))))
	       (t ;; atomic case
		nil))))
    (remove-duplicates (proper formula) 
		       :test #'(lambda (form-1 form-2)
				 (equal-formulas? form-1 form-2 nil)))))
	 
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

(defun instantiate (term variable formula)
  "Substitute TERM for free occurances of VARIBLE in FORMULA.

WARNING: No regard is given to variables appearing in TERM that may become
bound once the substitution is carried out: no renaming is done either
in TERM or FORMULA."
  (cond ((disjunction? formula)
	 (apply #'make-disjunction (mapcar #'(lambda (disjunct) (instantiate term variable disjunct))
					   (items formula))))
	((conjunction? formula)
	 (apply #'make-conjunction (mapcar #'(lambda (conjunct) (instantiate term variable conjunct))
					   (items formula))))
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