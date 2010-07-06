;;; formulas.lisp A representation for propositional and first-order formulas

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Keywords

(defclass formula ()
  nil)

(defun formula? (thing)
  (typep thing 'formula))

(defclass atomic-formula (formula)
  ((predicate :initarg :predicate
	      :accessor predicate)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defun atomic-formula? (thing)
  (typep thing 'atomic-formula))

(defmethod print-object ((atom atomic-formula) stream)
  (let ((pred (predicate atom))
	(args (arguments atom)))
    (if (null args)
	(format stream "~A" pred)
	(progn
	  (format stream "~A" pred)
	  (format stream "(")
	  (format stream "~A" (first args))
	  (loop for arg in (cdr args)
	     do
	       (format stream ",~A" arg))
	  (format stream ")")))))

(defgeneric make-atomic-formula (predicate &rest arguments))

(defmethod make-atomic-formula ((predicate symbol) &rest arguments)
  (make-instance 'atomic-formula
		 :predicate predicate
		 :args arguments))

(defmethod belongs-to-signature? ((sig signature) (formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (and (predicate? sig pred)
	 (every #'(lambda (arg)
		    (belongs-to-signature? sig arg))
		args))))

(unless (boundp 'contradiction)
  (defconstant contradiction (make-atomic-formula '⊥)))

(unless (boundp 'top)
  (defconstant top (make-atomic-formula '⊤)))

(defun make-equation (lhs rhs)
  (make-atomic-formula '= lhs rhs))

(defclass composite-formula (formula)
  nil)

(defun composite-formula? (formula)
  "Determine whether a formula is non-atomic.

Note that, unlike other predicates such as BINARY-DISJUNCTION? and
UNIVERSAL-GENERALIZATION?, this predicate does not merely test whether
the direct class of its argument is COMPOSITE-FORMULA.  The class
COMPOSITE-FORMULA is defined only to provide a common superclass for
further subclasses, such as BINARY-DISJUNCTION and
UNIVERSAL-GENERALIZATION, that is intended to be disjoint from the
class ATOMIC-FORMULA.  This function expresses that disjointedness."
  (and (formula? formula)
       (not (atomic-formula? formula))))

(defclass binary-connective-formula (composite-formula)
  ((lhs :initarg :lhs
	:accessor lhs
	:type formula)
   (rhs :initarg :rhs
	:accessor rhs
	:type formula)))

(defun binary-connective-formula? (thing)
  (typep thing 'binary-connective-formula))

(defmethod print-object :around ((formula binary-connective-formula) stream)
  (format stream "(~A " (lhs formula))
  (call-next-method)
  (format stream " ~A)" (rhs formula)))

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defmethod belongs-to-signature? ((sig signature)
				  (formula unary-connective-formula))
  (belongs-to-signature? sig (argument formula)))

(defmethod print-object :around ((formula unary-connective-formula) stream)
  (call-next-method)
  (format stream "~A" (argument formula)))

(defclass negation (unary-connective-formula)
  nil)

(defgeneric unnegate (formula))

(defmethod unnegate ((negation negation))
  (argument negation))

(defun negation? (thing)
  (typep thing 'negation))

(defmethod print-object ((neg negation) stream)
  (format stream "¬"))

(defgeneric negate (thing))

(defmethod negate ((formula formula))
  (make-instance 'negation :argument formula))

(defclass multiple-arity-connective-formula (composite-formula)
  ((items :initarg :items
	  :accessor items
	  :type list)))

(defgeneric connective-unit (multiple-arity-connective-formula))

(defmethod print-object :around ((formula multiple-arity-connective-formula)
				 stream)
  (let ((items (items formula)))
    (if (null items)
	(format stream "~A" (connective-unit formula))
	(if (null (cdr items))
	    (format stream "~A" (car items))
	    (progn
	      (format stream "(")
	      (format stream "~A" (car items))
	      (loop for item in (cdr items)
		   do
		   (format stream " ")
		   (call-next-method)
		   (format stream " ~A" item))
	      (format stream ")"))))))

(defmethod belongs-to-signature? ((sig signature)
				  (formula binary-connective-formula))
  (and (belongs-to-signature? sig (lhs formula))
       (belongs-to-signature? sig (rhs formula))))

(defmethod belongs-to-signature? ((sig signature)
				  (formula multiple-arity-connective-formula))
  (every #'(lambda (item)
	     (belongs-to-signature? sig item))
	 (items formula)))

(defclass implication (binary-connective-formula)
  nil)

(defun implication? (thing)
  (typep thing 'implication))

(defmethod print-object ((implication implication) stream)
  (format stream "→"))

(defgeneric make-implication (antecedent consequent))

(defmethod make-implication ((antecedent formula) (consequent formula))
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

(defun equivalence? (thing)
  (typep thing 'equivalence))

(defmethod print-object ((equiv equivalence) stream)
  (format stream "↔"))

(defun make-equivalence (lhs rhs)
  (make-instance 'equivalence
		 :lhs lhs
		 :rhs rhs))

;;; disjunctions

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defun binary-disjunction? (thing)
  (typep thing 'binary-disjunction))

(defmethod print-object ((bin-dis binary-disjunction) stream)
  (format stream "⋁"))

(defgeneric make-binary-disjunction (lhs rhs))

(defclass multiple-arity-disjunction (multiple-arity-connective-formula)
  nil)

(defmethod connective-unit ((mad multiple-arity-disjunction))
  (declare (ignore mad))
  top)

(defun multiple-arity-disjunction? (thing)
  (eql (class-of thing) 'multiple-arity-disjunction))

(defmethod print-object ((mad multiple-arity-disjunction) stream)
  (format stream "⋁"))

(defmethod make-binary-disjunction ((lhs formula) (rhs formula))
  (make-instance 'binary-disjunction
		 :lhs lhs
		 :rhs rhs))

(defgeneric make-multiple-arity-disjunction (&rest disjuncts))

(defmethod make-multiple-arity-disjunction (&rest disjuncts)
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

(defun binary-conjunction? (thing)
  (typep thing 'binary-conjunction))

(defmethod print-object ((con binary-conjunction) stream)
  (format stream "⋀"))

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defmethod connective-unit ((mac multiple-arity-conjunction))
  (declare (ignore mac))
  contradiction)

(defun multiple-arity-conjunction? (thing)
  (eql (class-of thing) 'multiple-arity-conjunction))

(defmethod print-object ((mac multiple-arity-conjunction) stream)
  (format stream "⋁"))

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

(defmethod print-object :after ((gen generalization) stream)
  (call-next-method)
  (format stream 
	  "~A[~A]"
	  (bound-variable gen)
	  (matrix gen)))

(defclass universal-generalization (generalization)
  nil)

(defun universal-generalization? (thing)
  (eql (class-of thing) 'universal-generalization))

(defmethod print-object ((uni-gen universal-generalization) stream)
  (format stream "∀"))

(defclass existential-generalization (generalization)
  nil)

(defun existential-generalization? (thing)
  (eql (class-of thing) 'existential-generalization))

(defmethod print-object ((exi-gen existential-generalization) stream)
  (format stream "∃"))

(defun make-universal (var formula)
  (make-instance 'universal-generalization
		 :bound-variable var
		 :matrix formula))

(defun make-existential (var formula)
  (make-instance 'existential-generalization
		 :bound-variable var
		 :matrix formula))

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

(define-condition expression-not-in-signature-error (error)
  ((expression :initarg :expression
	       :reader expression)
   (signature :initarg :signature
	      :reader signature))
  (:report (lambda (condition stream)
	     (let ((exp (expression condition))
		   (sig (signature condition)))
	       (format stream 
		       "The parsed expression~%~%  ~A~%~%does not belong to the signature~%~%  ~A"
		       exp
		       sig)))))

(define-condition incompatible-variable-kinds-error (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
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
				       (target-term function-term))
  (let ((f (function-symbol target-term))
	(args (arguments target-term)))
    (apply #'make-function-term 
	   f
	   (mapcar #'(lambda (x) (subst-term-for-var-in-term term var x))
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
;;; Substitutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substitution-value (subst var)
  (let ((bound (assoc var subst :test #'equal-variables?)))
    (if bound
	(cdr bound)
	:not-bound)))

(defun substitution-domain (subst)
  (mapcar #'car subst))

(defun remove-from-domain (subst var)
  (remove-if #'(lambda (v)
		 (equal-variables? v var))
	     subst
	     :key #'car))

(defun remove-all-from-domain (subst vars)
  (remove-if #'(lambda (v)
		 (member v vars :test #'equal-variables?))
	     subst
	     :key #'car))

(defgeneric apply-substitution (subst formula-or-term)
  (:documentation "Apply the substitution SUBST to FORMULA-OR-TERM."))

(defmethod apply-substitution (subst (formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (apply #'make-atomic-formula
	   pred
	   (mapcar #'(lambda (arg)
		       (apply-substitution subst arg))
		   args))))

(defmethod apply-substitution (subst (formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (apply-substitution subst (lhs formula))
		 :rhs (apply-substitution subst (rhs formula))))

(defmethod apply-substitution (subst (formula multiple-arity-connective-formula))
  (make-instance (class-of formula)
		 :items (mapcar #'(lambda (item)
				    (apply-substitution subst item))
				(items formula))))

(defmethod apply-substitution (subst (formula generalization))
  (let ((bound-var (bound-variable formula))
	(matrix (matrix formula)))
    (make-instance (class-of formula)
		   :bound-variable bound-var
		   :matrix (apply-substitution
			    (remove-from-domain subst bound-var)
			    matrix))))

(defmethod apply-substitution (subst (term variable-term))
  (let ((value (substitution-value subst term)))
    (if (eql value :not-bound)
	term
	value)))

(defmethod apply-substitution (subst (term function-term))
  (apply #'make-function-term
	 (function-symbol term)
	 (mapcar #'(lambda (subterm)
		     (apply-substitution subst subterm))
		 (arguments term))))

(defun compose-substitutions (subst-1 subst-2)
  "Compose the substitutions SUBST-1 and SUBST-2.  The order is
important; this function is not commutative.  The order of the
arguments to this function follows the ordinary definition of function
composition: with \"o\" as function composition, this function computes
SUBST-1 o SUBST-2, which, considered as a mathematical function, is
computed by taking an input x and applying it to SUBST-2 first, then
sending the output to SUBST-1."
  (let ((new-subst nil))
    (dolist (var-value-2 subst-2)
      (destructuring-bind (var-2 value-2)
	  var-value-2
	(setf new-subst
	      (acons var-2 
		     (apply-substitution subst-1 value-2)
		     new-subst))))
    (dolist (var-value-1 (remove-all-from-domain subst-1
						 (substitution-domain subst-2)))
      (destructuring-bind (var-1 value-1)
	  var-value-1
	(setf new-subst
	      (acons var-1 value-1 new-subst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unification of formulas and terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric occurs-in? (source target)
  (:documentation "Determine whether SOURCE, which can be either a term or a formula, occurs as a subformula/subterm of TARGET."))

(defmethod occurs-in? ((var variable-term) (term variable-term))
  (equal-variables? var term))

(defmethod occurs-in? ((source-term function-term) (var variable-term))
  nil)

(defmethod occurs-in? ((source-term function-term) (target-term function-term))
  (or (equal-terms? source-term target-term)
      (some #'(lambda (term)
		(occurs-in? source-term term))
	    (arguments target-term))))

(defmethod occurs-in? ((source-term term) (target atomic-formula))
  (some #'(lambda (term)
	    (occurs-in? source-term term))
	(arguments target)))

(defmethod occurs-in? ((source formula) (target term))
  nil)

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
  (if (some #'(lambda (ter)
		(occurs-in? var ter))
	    (arguments term))
      :fail
      (cons var term)))

(defmethod unify ((term function-term) (var variable-term))
  (if (occurs-in? var (arguments term))
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

(defun simple-substitution-for-var? (subst var)
  "Determine whether the substitution SUBST is a a simple substitution
that maps the variable VAR to a value, and maps nothing else to a
value."
  (and (not (null subst))
       (null (cdr subst))
       (let ((first-and-only (car subst)))
	 (equal-variables? (car first-and-only) var))))

(defun instance-of-quantified? (instantiated quantified-statement)
  "Determine whether INSTANTIATED is obtained from
  QUANTIFIED-STATEMENT, (ALL ?X A) or (EXISTS ?X A), by plugging in a
  term for the free occurences of ?X."
  (let ((mgu (unify instantiated (matrix quantified-statement))))
    (if (eql mgu :fail)
	nil
	(or (null mgu)
	    (let ((bound-var (bound-variable quantified-statement)))
	      (simple-substitution-for-var? mgu bound-var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sundry formula-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equal-formulas? (formula-1 formula-2))

(defmethod equal-formulas? ((form-1 atomic-formula) (form-2 atomic-formula))
  (and (eql (predicate form-1)
	    (predicate form-2))
       (every-pair #'(lambda (term-1 term-2)
		       (equal-terms? term-1 term-2))
		   (arguments form-1)
		   (arguments form-2))))

(defmethod equal-formulas? ((form-1 atomic-formula) (form-2 composite-formula))
  nil)

(defmethod equal-formulas? ((form-1 composite-formula) (form-2 atomic-formula))
  nil)

(defmethod equal-formulas? ((form-1 negation) (form-2 negation))
  (equal-formulas? (unnegate form-1)
		   (unnegate form-2)))

(defmethod equal-formulas? ((form-1 negation)
			    (form-2 binary-connective-formula))
  nil)

(defmethod equal-formulas? ((form-1 negation) 
			    (form-2 multiple-arity-connective-formula))
  nil)

(defmethod equal-formulas? ((form-1 negation) 
			    (form-2 generalization))
  nil)

(defmethod equal-formulas? ((form-1 binary-connective-formula)
			    (form-2 negation))
  nil)

(defmethod equal-formulas? ((form-1 multiple-arity-connective-formula)
			   (form-2 negation))
  nil)

(defmethod equal-formulas? ((form-1 generalization)
			   (form-2 negation))
  nil)

(defmethod equal-formulas? ((form-1 binary-connective-formula)
			    (form-2 multiple-arity-connective-formula))
  nil)

(defmethod equal-formulas? ((form-1 binary-connective-formula)
			    (form-2 generalization))
  nil)

(defmethod equal-formulas? ((form-1 multiple-arity-connective-formula)
			    (form-2 binary-connective-formula))
  nil)

(defmethod equal-formulas? ((form-1 multiple-arity-connective-formula)
			    (form-2 generalization))
  nil)

(defmethod equal-formulas? ((form-1 generalization)
			    (form-2 binary-connective-formula))
  nil)

(defmethod equal-formulas? ((form-1 generalization)
			    (form-2 multiple-arity-connective-formula))
  nil)

;; subclasses of binary-connective-formula

;; implication

(defmethod equal-formulas? ((form-1 implication)
			    (form-2 implication))
  (and (equal-formulas? (antecedent form-1)
			(antecedent form-2))
       (equal-formulas? (consequent form-1)
			(consequent form-2))))

(defmethod equal-formulas? ((form-1 implication)
			    (form-2 equivalence))
  nil)

(defmethod equal-formulas? ((form-1 implication)
			    (form-2 equivalence))
  nil)

(defmethod equal-formulas? ((form-1 implication)
			    (form-2 binary-disjunction))
  nil)

(defmethod equal-formulas? ((form-1 implication)
			   (form-2 binary-conjunction))
  nil)

;; equivalence

(defmethod equal-formulas? ((form-1 equivalence)
			    (form-2 implication))
  nil)

(defmethod equal-formulas? ((form-1 equivalence)
			    (form-2 equivalence))
  (and (equal-formulas? (lhs form-1)
			(lhs form-2))
       (equal-formulas? (rhs form-1)
			(rhs form-2))))

(defmethod equal-formulas? ((form-1 equivalence)
			    (form-2 binary-disjunction))
  nil)

(defmethod equal-formulas? ((form-1 equivalence)
			    (form-2 binary-conjunction))
  nil)

;; binary-disjunction

(defmethod equal-formulas? ((form-1 binary-disjunction)
			    (form-2 implication))
  nil)

(defmethod equal-formulas? ((form-1 binary-disjunction)
			    (form-2 equivalence))
  nil)

(defmethod equal-formulas? ((form-1 binary-disjunction)
			    (form-2 binary-disjunction))
  (and (equal-formulas? (lhs form-1)
			(lhs form-2))
       (equal-formulas? (rhs form-1)
			(rhs form-2))))

(defmethod equal-formulas? ((form-1 binary-disjunction)
			    (form-2 binary-conjunction))
  nil)

;; binary-conjunction

(defmethod equal-formulas? ((form-1 binary-conjunction)
			    (form-2 implication))
  nil)

(defmethod equal-formulas? ((form-1 binary-conjunction)
			    (form-2 equivalence))
  nil)

(defmethod equal-formulas? ((form-1 binary-conjunction)
			    (form-2 binary-disjunction))
  nil)

(defmethod equal-formulas? ((form-1 binary-conjunction)
			    (form-2 binary-conjunction))
  (and (equal-formulas? (lhs form-1)
			(lhs form-2))
       (equal-formulas? (rhs form-1)
			(rhs form-2))))

;; multiple-arity-disjunction

(defmethod equal-formulas? ((form-1 multiple-arity-disjunction)
			(form-2 multiple-arity-disjunction))
  (every-pair #'(lambda (item-1 item-2)
		  (equal-formulas? item-1 item-2))
	      (items form-1)
	      (items form-2)))

(defmethod equal-formulas? ((form-1 multiple-arity-disjunction)
			    (form-2 multiple-arity-conjunction))
  nil)

;; multiple-arity-conjunction

(defmethod equal-formulas? ((form-1 multiple-arity-conjunction)
			    (form-2 multiple-arity-disjunction))
  nil)

(defmethod equal-formulas? ((form-1 multiple-arity-conjunction)
			    (form-2 multiple-arity-conjunction))
  (every-pair #'(lambda (item-1 item-2)
		  (equal-formulas? item-1 item-2))
	      (items form-1)
	      (items form-2)))

(defun contains-formula? (lst formula)
  (member formula lst :test #'equal-formulas?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition malformed-formula-error (error)
  ((text :initarg :text 
	 :reader malformed-formula-error-text))
  (:report (lambda (condition stream)
	     (let ((text (malformed-formula-error-text condition)))
	       (if (null text)
		   (format stream
			   "Weird: no text was given.~%")
		   (format stream 
			   "The given text,~%~%  ~A,~%~%is not a formula."
			   text))))))

(define-condition parse-form-empty-argument-list-error (error)
  ((op :initarg :op
       :reader operator))
  (:report (lambda (condition stream)
	     (let ((op (operator condition)))
	       (format stream 
		       "The operator ~A expects at least one argument, but none were supplied."
		       op)))))		       

(define-condition parse-form-at-least-two-args-expected-but-only-one-supplied-error (error)
  ((op :initarg :op
       :reader operator)
   (first-arg :initarg :first-arg
	      :reader first-argument))
  (:report (lambda (condition stream)
	     (let ((op (operator condition))
		   (first-arg (first-argument condition)))
	       (format stream 
		       "The operator ~A requires at least two arguments, but only one,~%~%  ~A,~%~%was supplied."
		       op first-arg)))))

(define-condition parse-form-unary-operator-multiple-arguments-error (error)
  ((op :initarg :op
       :reader operator)
   (args :initarg :args
	 :reader arguments))
  (:report (lambda (condition stream)
	     (let* ((op (operator condition))
		    (args (arguments condition))
		    (num-args (length args)))
	       (format stream 
		       "The unary operator ~A expects exactly one argument, but multiple arguments were supplied:~%~%"
		       op)
	       (loop 
		  for i from 1 upto num-args
		  for arg in args
		  do
		    (format stream "[~d] ~A" i arg))))))

(define-condition parse-form-exactly-two-args-expected-but-at-least-three-supplied-error (error)
  ((op :initarg op
       :reader operator)
   (args :initarg :args
	 :reader arguments))
  (:report (lambda (condition stream)
	     (let* ((op (operator condition))
		    (args (arguments condition))
		    (num-args (length args)))
	       (format stream "The binary operator ~A expects exactly two arguments, but at least three arguments were supplied:~%~%" op)
	       (loop 
		  for i from 1 upto num-args
		  for arg in args
		  do
		    (format stream "[~d] ~A" i arg))))))

(define-condition parse-form-formula-expected-error (error)
  ((op :initarg :op
       :reader operator)
   (form :initarg :form
	 :reader form))
  (:report (lambda (condition stream)
	     (let ((op (operator condition))
		   (form (form condition)))
	     (format stream "The operator ~A expects an formula for one of its arguments, but a non-formula was given in that argument position.~%" op)
	     (format stream "The given argument was~%~%  ~A~%~%This form cannot be understood as a formula." form)))))

(define-condition parse-form-variable-expected (error)
  ((op :initarg :op
       :reader operator)
   (form :initarg :form
	 :reader form))
  (:report (lambda (condition stream)
	     (let ((op (operator condition))
		   (form (form condition)))
	     (format stream "The operator ~A expects an variable for one of its arguments, but a non-variable was given in that argument position.~%" op)
	     (format stream "The given argument was~%~%  ~A~%~%This form cannot be understood as a variable." form)))))

(define-condition parse-form-empty-list-supplied-error (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "The empty list cannot be understood as either a formula or a term."))))

(defun try-another-formula (c)
  (declare (ignore c))
  (let ((restart (find-restart 'try-another-formula)))
    (when restart
      (invoke-restart 'try-another-formula))))

(defgeneric form->formula (thing)
  (:documentation "Given THING, try to make sense of it as a formula."))

(defgeneric op-and-args->formula (operator arguments)
  (:documentation "Try to understand a symbol OPERATOR and a list ARGUMENTS as a formula."))

(defgeneric op-and-args->term (operator arguments)
  (:documentation "Try to understand a symbol OPERATOR and a list ARGUMENTS as a term."))

(defmethod op-and-args->formula ((op (eql 'or)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op
		 :first-arg (car arguments))
	  (if (null (cddr arguments))
	      (let ((first-disjunct (form->formula (car arguments)))
		    (second-disjunct (form->formula (cadr arguments))))
		(make-binary-disjunction first-disjunct second-disjunct))
	      (let ((disjuncts (mapcar #'form->formula arguments)))
		(apply #'make-multiple-arity-disjunction disjuncts))))))

(defmethod op-and-args->formula ((op (eql 'and)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((first-conjunct (form->formula (car arguments)))
		    (second-conjunct (form->formula (cadr arguments))))
		(make-binary-conjunction first-conjunct second-conjunct))
	      (let ((conjuncts (mapcar #'form->formula arguments)))
		(apply #'make-multiple-arity-conjunction conjuncts))))))

(defmethod op-and-args->formula ((op (eql 'not)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (let ((negated (form->formula (car arguments))))
	    (negate negated))
	  (error 'parse-form-unary-operator-multiple-arguments-error
		 :operator op))))

(defmethod op-and-args->formula ((op (eql 'implies)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((antecedent (form->formula (car arguments)))
		    (consequent (form->formula (cadr arguments))))
		(make-implication antecedent consequent))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op
		     :args arguments)))))

(defmethod op-and-args->formula ((op (eql 'iff)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((lhs (form->formula (car arguments)))
		    (rhs (form->formula (cadr arguments))))
		(make-equivalence lhs rhs))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod op-and-args->formula ((op (eql 'all)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((var (form->formula (car arguments))))
		(if (eql (class-of var) 'unsorted-variable-term)
		    (let ((matrix (form->formula (cadr arguments))))
		      (if (eql (class-of matrix) 'formula)
			  (make-universal var matrix)
			  (error 'parse-form-formula-expected-error
				 :operator op
				 :form (cadr arguments))))
		    (error 'parse-form-variable-expected
			   :operator op
			   :form (car arguments))))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod op-and-args->formula ((op (eql 'all)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((var (form->term (car arguments))))
		(if (eql (class-of var) 'unsorted-variable-term)
		    (let ((matrix (form->formula (cadr arguments))))
		      (if (eql (class-of matrix) 'formula)
			  (make-universal var matrix)
			  (error 'parse-form-formula-expected-error
				 :operator op
				 :form (cadr arguments))))
		    (error 'parse-form-variable-expected
			   :operator op
			   :form (car arguments))))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod op-and-args->formula ((op (eql 'exists)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((var (form->formula (car arguments))))
		(if (eql (class-of var) 'unsorted-variable-term)
		    (let ((matrix (form->term (cadr arguments))))
		      (if (eql (class-of matrix) 'formula)
			  (make-universal var matrix)
			  (error 'parse-form-formula-expected-error
				 :operator op
				 :form (cadr arguments))))
		    (error 'parse-form-variable-expected
			   :operator op
			   :form (car arguments))))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod form->formula ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (let ((first (first list)))
	(op-and-args->formula first (cdr list)))))

(defmethod op-and-args->term ((op symbol) arguments)
  (let ((arguments-as-terms (mapcar #'form->term arguments)))
    (apply #'make-function-term
	   op
	   arguments-as-terms)))

(defmethod form->formula ((sym symbol))
  (make-atomic-formula sym))

(defgeneric form->term (form)
  (:documentation "Attempt to understand FORM as a term."))

(defmethod form->term ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (op-and-args->term (car list) (cdr list))))

(defmethod form->term ((sym symbol))
  (let ((name (symbol-name sym)))
    (if (empty-string? name)
	(error 'parse-form-empty-string-supplied)
	(let ((first-char (char name 0)))
	  (if (char= first-char #\?)
	      (make-variable (subseq name 1))
	      (make-function-term name))))))

(defmethod negate ((sym symbol))
  (negate (form->formula sym)))

(defmethod make-implication ((antecedent symbol) (consequent symbol))
  (make-implication (form->formula antecedent)
		    (form->formula consequent)))

(defmethod make-implication ((antecedent symbol) (consequent formula))
  (make-implication (form->formula antecedent)
		    consequent))

(defmethod make-implication ((antecedent formula) (consequent symbol))
  (make-implication antecedent
		    (form->formula consequent)))

(defmethod make-binary-disjunction ((lhs symbol) (rhs symbol))
  (make-binary-disjunction (form->formula lhs)
			   (form->formula rhs)))

(defmethod make-binary-disjunction ((lhs symbol) (rhs formula))
  (make-binary-disjunction (form->formula lhs)
			   rhs))

(defmethod make-binary-disjunction ((lhs formula) (rhs symbol))
  (make-binary-disjunction lhs
			   (form->formula rhs)))

(defun read-formula (&optional (stream *standard-input*))
  (let ((input-form (read stream nil nil)))
    (let ((formula (form->formula input-form)))
      (if (formula? formula)
	  formula
	  (error 'malformed-formula-error :text input-form)))))

(defun parse-formula (str)
  (with-input-from-string (s str)
    (read-formula s)))

(defun read-new-formula ()
  (format t "Enter a new formula: ")
  (multiple-value-list (read-formula)))

(defun read-atomic-formula ()
  (let (response)
    (until (atomic-formula? response)
      (read-formula))
    response))

(define-condition non-atomic-formula-error (error)
  ((text :initarg :text 
	 :reader non-atomic-formula-error-text))
  (:report (lambda (condition stream)
	     (let ((text (non-atomic-formula-error-text condition)))
	       (if (null text)
		   (format stream
			   "Weird: no text was given (or text is simply NIL)")
		   (format stream 
			   "The given text,~%~%  ~A,~%~%is an atomic formula."
			   text))))))

(defun read-composite-formula (&optional (stream *standard-input*))
  (let ((input (read-formula stream)))
    (if (atomic-formula? input)
	(error 'non-atomic-formula-error :text input)
	input)))

(defun read-composite-formula-in-signature (signature 
					    &optional (stream *standard-input*))
  (let ((formula (read-composite-formula stream)))
    (if (belongs-to-signature? signature formula)
	formula
	(error 'expression-not-in-signature-error
	       :expression formula))))

;;; formulas.lisp ends here