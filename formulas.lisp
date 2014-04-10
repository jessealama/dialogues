;;; formulas.lisp A representation for propositional and first-order formulas

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defclass negation (unary-connective-formula)
  nil)

(defclass binary-connective-formula (composite-formula)
  ((lhs :initarg :lhs
	:accessor lhs
	:type formula)
   (rhs :initarg :rhs
	:accessor rhs
	:type formula)))

(defclass binary-conjunction (binary-connective-formula)
  nil)

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defclass implication (binary-connective-formula)
  nil)

(defclass equivalence (binary-connective-formula)
  nil)

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

(defgeneric render-plainly (statement))

(defgeneric render-fancily (statement))

(defmethod render-plainly ((statement term))
  (let ((func-sym (function-symbol statement))
	(args (arguments statement)))
    (if (null args)
	(format nil "~A" func-sym)
	(if (null (cdr args))
	    (format nil "~A(~A)"
		    func-sym
		    (render-plainly (car args)))
	    (funcall #'concat-strings
		     (format nil "~A" func-sym)
		     "("
		     (render-plainly (car args))
		     (apply #'concat-strings
			    (mapcar #'(lambda (arg)
					(format nil ",~A" (render-plainly arg)))
				    (cdr args)))
		     ")")))))

(defmethod render-fancily ((statement term))
  (render-plainly statement))

(defmethod render-plainly :around ((formula unary-connective-formula))
  (let ((body (call-next-method)))
    (concatenate 'string body (render-plainly (argument formula)))))

(defmethod render-fancily :around ((formula unary-connective-formula))
  (format nil "~a~a" (call-next-method) (render-fancily (argument formula))))

(defmethod render-plainly ((neg negation))
  "~")

(defmethod render-fancily ((neg negation))
  "¬")

(defmethod render-plainly :around ((formula binary-connective-formula))
  (concatenate 'string
	       "("
	       (render-plainly (lhs formula))
	       " "
	       (call-next-method)
	       " "
	       (render-plainly (rhs formula))
	       ")"))

(defmethod render-fancily :around ((formula binary-connective-formula))
  (format nil "(~a ~a ~a)"
	  (render-fancily (lhs formula))
	  (call-next-method)
	  (render-fancily (rhs formula))))

(defmethod render-plainly :around ((gen generalization))
  (concatenate 'string
	       (call-next-method)
	       (render-plainly (bound-variable gen))
	       "["
	       (render-plainly (matrix gen))
	       "]"))

(defmethod render-fancily :around ((gen generalization))
  (format nil "~a~a[~a]"
	  (call-next-method)
	  (render-fancily (bound-variable gen))
	  (render-fancily (matrix gen))))

(defmethod render-plainly ((formula binary-conjunction))
  "&")

(defmethod render-fancily ((formula binary-conjunction))
  "∧")

(defmethod render-plainly ((formula binary-disjunction))
  "v")

(defmethod render-fancily ((formula binary-disjunction))
  "∨")

(defmethod render-plainly ((formula implication))
  "-->")

(defmethod render-fancily ((formula implication))
  "→")

(defmethod render-plainly ((formula equivalence))
  "<-->")

(defmethod render-fancily ((formula equivalence))
  "↔")

(defmethod render-plainly ((formula universal-generalization))
  "forall")

(defmethod render-fancily ((formula universal-generalization))
  "∀")

(defmethod render-plainly ((formula existential-generalization))
  "exists")

(defmethod render-fancily ((formula existential-generalization))
  "∃")

(defmethod render-plainly ((formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (if (null args)
	(format nil "~(~a~)" pred)
	(if (null (cdr args))
	    (format nil "~(~a~)(~a)"
		    pred
		    (render-plainly (car args)))
	    (funcall #'concat-strings
		     (format nil "~A" pred)
		     "("
		     (render-plainly (car args))
		     (apply #'concatenate
			    'string
			    (mapcar #'(lambda (arg)
					(format nil ",~A" (render-plainly arg)))
				    (cdr args)))
		     ")")))))

(defmethod render-fancily ((formula atomic-formula))
  (format nil "<i>~a</i>" (render-plainly formula)))

(defgeneric make-atomic-formula (predicate &rest arguments))

(let ((atomic-formula-store (make-hash-table)))
  (defmethod make-atomic-formula ((predicate symbol) &rest arguments)
    (or (gethash predicate atomic-formula-store)
	(setf (gethash predicate atomic-formula-store)
	      (make-instance 'atomic-formula
			     :predicate predicate
			     :args arguments)))))

(defmethod belongs-to-signature? ((sig signature) (formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (and (predicate? sig pred)
	 (every #'(lambda (arg)
		    (belongs-to-signature? sig arg))
		args))))

(defparameter contradiction (make-atomic-formula 'bottom))

(defparameter top (make-atomic-formula 'top))

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

(defun binary-connective-formula? (thing)
  (typep thing 'binary-connective-formula))

(defmethod belongs-to-signature? ((sig signature)
				  (formula unary-connective-formula))
  (belongs-to-signature? sig (argument formula)))

(defgeneric unnegate (formula))

(defmethod unnegate ((negation negation))
  (argument negation))

(defun negation? (thing)
  (typep thing 'negation))

(defgeneric negate (thing))

(defmethod negate ((formula formula))
  (make-instance 'negation :argument formula))

(defclass multiple-arity-connective-formula (composite-formula)
  ((items :initarg :items
	  :accessor items
	  :type list)))

(defgeneric connective-unit (multiple-arity-connective-formula))

(defmethod belongs-to-signature? ((sig signature)
				  (formula binary-connective-formula))
  (and (belongs-to-signature? sig (lhs formula))
       (belongs-to-signature? sig (rhs formula))))

(defmethod belongs-to-signature? ((sig signature)
				  (formula multiple-arity-connective-formula))
  (every #'(lambda (item)
	     (belongs-to-signature? sig item))
	 (items formula)))

(defun implication? (thing)
  (typep thing 'implication))

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

(defun equivalence? (thing)
  (typep thing 'equivalence))

(defun make-equivalence (lhs rhs)
  (make-instance 'equivalence
		 :lhs lhs
		 :rhs rhs))

;;; disjunctions

(defun binary-disjunction? (thing)
  (typep thing 'binary-disjunction))

(defgeneric make-binary-disjunction (lhs rhs))

(defclass multiple-arity-disjunction (multiple-arity-connective-formula)
  nil)

(defmethod connective-unit ((mad multiple-arity-disjunction))
  (declare (ignore mad))
  top)

(defun multiple-arity-disjunction? (thing)
  (eql (class-of thing) 'multiple-arity-disjunction))

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

(defun binary-conjunction? (thing)
  (typep thing 'binary-conjunction))

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defmethod connective-unit ((mac multiple-arity-conjunction))
  (declare (ignore mac))
  contradiction)

(defun multiple-arity-conjunction? (thing)
  (eql (class-of thing) 'multiple-arity-conjunction))

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

(defun universal-generalization? (thing)
  (eql (class-of thing) 'universal-generalization))

(defun existential-generalization? (thing)
  (eql (class-of thing) 'existential-generalization))

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

(defun proper-subformula-occurrences (formula)
  (proper-subformulas-1 formula))

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

(defun substitution-value (subst var &key (test #'equal-variables?))
  (let ((bound (assoc var subst :test test)))
    (if bound
	(cdr bound)
	:not-bound)))

(defun substitution-domain (subst)
  (mapcar #'car subst))

(defun substitution-range (subst)
  (mapcar #'cdr subst))

(defun complete-substitution (subst additional-vars)
  "A substitution that is just like SUBST, but which explicitly maps
variables in ADDITIONAL-VARS, if not already present in the domain of
SUBST, to themselves."
  (append subst
	  (mapcar #'(lambda (var)
		      (cons var var))
		  (remove-if #'(lambda (var)
				 (assoc var subst))
			     additional-vars))))

(defun remove-from-domain (subst var)
  (remove-if #'(lambda (v)
		 (equal-variables? v var))
	     subst
	     :key #'car))

(defun remove-all-from-domain (subst vars &key (test #'equal-variables?))
  (remove-if #'(lambda (v)
		 (member v vars :test test))
	     subst
	     :key #'car))

(defun compatible-substitutions? (subst-1 subst-2)
  (or (equalp subst-1 subst-2)
      (null (intersection (substitution-domain subst-1)
			  (substitution-domain subst-2)))))

(defgeneric apply-substitution (subst formula-or-term &key test)
  (:documentation "Apply the substitution SUBST to FORMULA-OR-TERM."))

(defmethod apply-substitution ((subst (eql nil)) formula &key test)
  (declare (ignore test))
  formula)

(defmethod apply-substitution (subst (formula atomic-formula) &key test)
  (let ((subst-val (substitution-value subst formula :test test)))
    (if (eq subst-val :not-bound)
	formula
	subst-val)))

(defmethod apply-substitution (subst (formula unary-connective-formula) &key test)
  (make-instance (class-of formula)
		 :argument (apply-substitution subst (argument formula) :test test)))

(defmethod apply-substitution (subst (formula binary-connective-formula) &key test)
  (make-instance (class-of formula)
		 :lhs (apply-substitution subst (lhs formula) :test test)
		 :rhs (apply-substitution subst (rhs formula) :test test)))

(defmethod apply-substitution (subst (formula multiple-arity-connective-formula) &key test)
  (make-instance (class-of formula)
		 :items (mapcar #'(lambda (item)
				    (apply-substitution subst item :test test))
				(items formula))))

(defmethod apply-substitution (subst (formula generalization) &key test)
  (let ((bound-var (bound-variable formula))
	(matrix (matrix formula)))
    (make-instance (class-of formula)
		   :bound-variable bound-var
		   :matrix (apply-substitution
			    (remove-from-domain subst bound-var)
			    matrix
			    :test test))))

(defmethod apply-substitution (subst (term variable-term) &key test)
  (let ((value (substitution-value subst term :test test)))
    (if (eql value :not-bound)
	term
	value)))

(defmethod apply-substitution (subst (term function-term) &key test)
  (apply #'make-function-term
	 (function-symbol term)
	 (mapcar #'(lambda (subterm)
		     (apply-substitution subst subterm :test test))
		 (arguments term))))

(defun compose-substitutions (subst-1 subst-2 &key (test #'equal-variables?))
  "Compose the substitutions SUBST-1 and SUBST-2.  The order is
important; this function is not commutative.  The order of the
arguments to this function follows the ordinary definition of function
composition: with \"o\" as function composition, this function computes
SUBST-1 o SUBST-2, which, considered as a mathematical function, is
computed by taking an input x and applying it to SUBST-2 first, then
sending the output to SUBST-1."
  (let ((new-subst nil))
    (dolist (var-value-2 subst-2)
      (destructuring-bind (var-2 . value-2)
	  var-value-2
	(setf new-subst
	      (acons var-2
		     (apply-substitution subst-1 value-2 :test test)
		     new-subst))))
    (dolist (var-value-1 (remove-all-from-domain subst-1
						 (substitution-domain subst-2)
						 :test test)
	     new-subst)
      (destructuring-bind (var-1 . value-1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unification of propositional formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unify-propositional-formulas-simply (formula-1 formula-2))

(defmethod unify-propositional-formulas-simply ((formula-1 atomic-formula)
						(formula-2 atomic-formula))
  (if (equal-formulas? formula-1 formula-2)
      nil
      (acons formula-1 formula-2 nil)))

(defmethod unify-propositional-formulas-simply ((formula-1 atomic-formula)
						(formula-2 negation))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 atomic-formula)
						(formula-2 binary-conjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 atomic-formula)
						(formula-2 binary-disjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 atomic-formula)
						(formula-2 implication))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 atomic-formula)
						(formula-2 equivalence))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 negation)
						(formula-2 atomic-formula))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 negation)
						(formula-2 negation))
  (unify-propositional-formulas-simply (argument formula-1)
				       (argument formula-2)))

(defmethod unify-propositional-formulas-simply ((formula-1 negation)
						(formula-2 binary-conjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 negation)
						(formula-2 binary-disjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 negation)
						(formula-2 implication))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 negation)
						(formula-2 equivalence))
  :fail)

(defun unify-binary-connective-propositional-formulas-simply (formula-1 formula-2)
  (let* ((lhs-1 (lhs formula-1))
	 (lhs-2 (lhs formula-2))
	 (mgu-lhs (unify-propositional-formulas-simply lhs-1 lhs-2)))
    (if (eq mgu-lhs :fail)
	:fail
	(let ((mgu-rhs (unify-propositional-formulas-simply
			(apply-substitution mgu-lhs (rhs formula-1) :test #'equal-atomic-formulas?)
			(apply-substitution mgu-lhs (rhs formula-2) :test #'equal-atomic-formulas?))))
	  (if (eq mgu-rhs :fail)
	      :fail
	      (compose-substitutions mgu-lhs
				     mgu-rhs
				     :test #'equal-atomic-formulas?))))))

(defmethod unify-propositional-formulas-simply ((formula-1 binary-conjunction)
						(formula-2 atomic-formula))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-conjunction)
						(formula-2 negation))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-conjunction)
						(formula-2 binary-conjunction))
  (unify-binary-connective-propositional-formulas-simply formula-1
							 formula-2))

(defmethod unify-propositional-formulas-simply ((formula-1 binary-conjunction)
						(formula-2 binary-disjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-conjunction)
						(formula-2 implication))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-conjunction)
						(formula-2 equivalence))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-disjunction)
						(formula-2 atomic-formula))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-disjunction)
						(formula-2 negation))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-disjunction)
						(formula-2 binary-conjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-disjunction)
						(formula-2 binary-disjunction))
  (unify-binary-connective-propositional-formulas-simply formula-1
							 formula-2))

(defmethod unify-propositional-formulas-simply ((formula-1 binary-disjunction)
						(formula-2 implication))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 binary-disjunction)
						(formula-2 equivalence))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 implication)
						(formula-2 atomic-formula))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 implication)
						(formula-2 negation))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 implication)
						(formula-2 binary-conjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 implication)
						(formula-2 binary-disjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 implication)
						(formula-2 implication))
  (unify-binary-connective-propositional-formulas-simply formula-1
							 formula-2))

(defmethod unify-propositional-formulas-simply ((formula-1 implication)
						(formula-2 equivalence))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 equivalence)
					       (formula-2 atomic-formula))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 equivalence)
						(formula-2 negation))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 equivalence)
						(formula-2 binary-conjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 equivalence)
						(formula-2 binary-disjunction))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 equivalence)
						(formula-2 implication))
  :fail)

(defmethod unify-propositional-formulas-simply ((formula-1 equivalence)
						(formula-2 equivalence))
  (unify-binary-connective-propositional-formulas-simply formula-1
							 formula-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matchings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric match-formulas (formula-1 formula-2))

(defmethod match-formulas ((formula-1 atomic-formula)
						(formula-2 atomic-formula))
  (if (equal-formulas? formula-1 formula-2)
      nil
      (acons formula-1 formula-2 nil)))

(defmethod match-formulas ((formula-1 atomic-formula)
			   (formula-2 negation))
  :fail)

(defmethod match-formulas ((formula-1 atomic-formula)
			   (formula-2 binary-conjunction))
  :fail)

(defmethod match-formulas ((formula-1 atomic-formula)
			   (formula-2 binary-disjunction))
  :fail)

(defmethod match-formulas ((formula-1 atomic-formula)
			   (formula-2 implication))
  :fail)

(defmethod match-formulas ((formula-1 atomic-formula)
			   (formula-2 equivalence))
  :fail)

(defmethod match-formulas ((formula-1 negation)
			   (formula-2 atomic-formula))
  :fail)

(defmethod match-formulas ((formula-1 negation)
			   (formula-2 negation))
  (match-formulas (argument formula-1)
		  (argument formula-2)))

(defmethod match-formulas ((formula-1 negation)
			   (formula-2 binary-conjunction))
  :fail)

(defmethod match-formulas ((formula-1 negation)
			   (formula-2 binary-disjunction))
  :fail)

(defmethod match-formulas ((formula-1 negation)
			   (formula-2 implication))
  :fail)

(defmethod match-formulas ((formula-1 negation)
			   (formula-2 equivalence))
  :fail)

(defun match-binary-connective-propositional-formulas (formula-1 formula-2)
  (let* ((lhs-1 (lhs formula-1))
	 (lhs-2 (lhs formula-2))
	 (match-lhs (match-formulas lhs-1 lhs-2)))
    (if (eq match-lhs :fail)
	:fail
	(let ((match-rhs (match-formulas (rhs formula-1) (rhs formula-2))))
	  (if (eq match-rhs :fail)
	      :fail
	      (if (compatible-substitutions? match-lhs match-rhs)
		  (append match-lhs match-rhs)
		  :fail))))))

(defmethod match-formulas ((formula-1 binary-conjunction)
						(formula-2 atomic-formula))
  :fail)

(defmethod match-formulas ((formula-1 binary-conjunction)
						(formula-2 negation))
  :fail)

(defmethod match-formulas ((formula-1 binary-conjunction)
						(formula-2 binary-conjunction))
  (match-binary-connective-propositional-formulas formula-1
						  formula-2))

(defmethod match-formulas ((formula-1 binary-conjunction)
						(formula-2 binary-disjunction))
  :fail)

(defmethod match-formulas ((formula-1 binary-conjunction)
						(formula-2 implication))
  :fail)

(defmethod match-formulas ((formula-1 binary-conjunction)
						(formula-2 equivalence))
  :fail)

(defmethod match-formulas ((formula-1 binary-disjunction)
						(formula-2 atomic-formula))
  :fail)

(defmethod match-formulas ((formula-1 binary-disjunction)
						(formula-2 negation))
  :fail)

(defmethod match-formulas ((formula-1 binary-disjunction)
						(formula-2 binary-conjunction))
  :fail)

(defmethod match-formulas ((formula-1 binary-disjunction)
						(formula-2 binary-disjunction))
  (match-binary-connective-propositional-formulas formula-1
						  formula-2))

(defmethod match-formulas ((formula-1 binary-disjunction)
						(formula-2 implication))
  :fail)

(defmethod match-formulas ((formula-1 binary-disjunction)
						(formula-2 equivalence))
  :fail)

(defmethod match-formulas ((formula-1 implication)
						(formula-2 atomic-formula))
  :fail)

(defmethod match-formulas ((formula-1 implication)
						(formula-2 negation))
  :fail)

(defmethod match-formulas ((formula-1 implication)
						(formula-2 binary-conjunction))
  :fail)

(defmethod match-formulas ((formula-1 implication)
						(formula-2 binary-disjunction))
  :fail)

(defmethod match-formulas ((formula-1 implication)
						(formula-2 implication))
  (match-binary-connective-propositional-formulas formula-1
						  formula-2))

(defmethod match-formulas ((formula-1 implication)
						(formula-2 equivalence))
  :fail)

(defmethod match-formulas ((formula-1 equivalence)
					       (formula-2 atomic-formula))
  :fail)

(defmethod match-formulas ((formula-1 equivalence)
						(formula-2 negation))
  :fail)

(defmethod match-formulas ((formula-1 equivalence)
						(formula-2 binary-conjunction))
  :fail)

(defmethod match-formulas ((formula-1 equivalence)
						(formula-2 binary-disjunction))
  :fail)

(defmethod match-formulas ((formula-1 equivalence)
						(formula-2 implication))
  :fail)

(defmethod match-formulas ((formula-1 equivalence)
						(formula-2 equivalence))
  (match-binary-connective-propositional-formulas formula-1
						  formula-2))

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

(defun equal-atomic-formulas? (formula-1 formula-2)
  (and (atomic-formula? formula-1)
       (atomic-formula? formula-2)
       (equal-formulas? formula-1 formula-2)))

(defgeneric contains-contradiction-p (x)
  (:documentation "Is a contradiction (bottom) found anywhere inside X?"))

(defmethod contains-contradiction-p ((x atomic-formula))
  (string= (predicate x) "false"))

(defmethod contains-contradiction-p ((x negation))
  (contains-contradiction-p (argument x)))

(defmethod contains-contradiction-p ((x binary-conjunction))
  (or (contains-contradiction-p (lhs x))
      (contains-contradiction-p (rhs x))))

(defmethod contains-contradiction-p ((x binary-disjunction))
  (or (contains-contradiction-p (lhs x))
      (contains-contradiction-p (rhs x))))

(defmethod contains-contradiction-p ((x implication))
  (or (contains-contradiction-p (antecedent x))
      (contains-contradiction-p (consequent x))))

(defmethod contains-contradiction-p ((x equivalence))
  (or (contains-contradiction-p (lhs x))
      (contains-contradiction-p (rhs x))))

(defmethod contains-contradiction-p ((x multiple-arity-conjunction))
  (some #'contains-contradiction-p (arguments x)))

(defmethod contains-contradiction-p ((x multiple-arity-disjunction))
  (some #'contains-contradiction-p (arguments x)))

(defmethod contains-contradiction-p ((x generalization))
  (contains-contradiction-p (matrix x)))

(defgeneric contains-verum-p (x)
  (:documentation "Is a verum (top) found anywhere inside X?"))

(defmethod contains-verum-p ((x atomic-formula))
  (string= (predicate x) "true"))

(defmethod contains-verum-p ((x negation))
  (contains-verum-p (argument x)))

(defmethod contains-verum-p ((x binary-conjunction))
  (or (contains-verum-p (lhs x))
      (contains-verum-p (rhs x))))

(defmethod contains-verum-p ((x binary-disjunction))
  (or (contains-verum-p (lhs x))
      (contains-verum-p (rhs x))))

(defmethod contains-verum-p ((x implication))
  (or (contains-verum-p (antecedent x))
      (contains-verum-p (consequent x))))

(defmethod contains-verum-p ((x equivalence))
  (or (contains-verum-p (lhs x))
      (contains-verum-p (rhs x))))

(defmethod contains-verum-p ((x generalization))
  (contains-verum-p (matrix x)))

(defmethod contains-verum-p ((x multiple-arity-conjunction))
  (some #'contains-verum-p (arguments x)))

(defmethod contains-verum-p ((x multiple-arity-disjunction))
  (some #'contains-verum-p (arguments x)))

(defgeneric contains-quantifier-p (x)
  (:documentation "Is a quantifier found anywhere inside X?"))

(defmethod contains-quantifier-p ((x atomic-formula))
  nil)

(defmethod contains-quantifier-p ((x negation))
  (contains-quantifier-p (argument x)))

(defmethod contains-quantifier-p ((x binary-conjunction))
  (or (contains-quantifier-p (lhs x))
      (contains-quantifier-p (rhs x))))

(defmethod contains-quantifier-p ((x binary-disjunction))
  (or (contains-quantifier-p (lhs x))
      (contains-quantifier-p (rhs x))))

(defmethod contains-quantifier-p ((x implication))
  (or (contains-quantifier-p (antecedent x))
      (contains-quantifier-p (consequent x))))

(defmethod contains-quantifier-p ((x equivalence))
  (or (contains-quantifier-p (lhs x))
      (contains-quantifier-p (rhs x))))

(defmethod contains-quantifier-p ((x multiple-arity-conjunction))
  (some #'contains-quantifier-p (arguments x)))

(defmethod contains-quantifier-p ((x multiple-arity-disjunction))
  (some #'contains-quantifier-p (arguments x)))

(defmethod contains-quantifier-p ((x generalization))
  t)

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
	(op-and-args->formula (symbolify-here first)
			      (cdr list)))))

(defmethod op-and-args->term ((op symbol) arguments)
  (let ((arguments-as-terms (mapcar #'form->term arguments)))
    (apply #'make-function-term
	   op
	   arguments-as-terms)))

(defmethod form->formula ((sym symbol))
  (make-atomic-formula (symbolify-here sym)))

(defgeneric form->term (form)
  (:documentation "Attempt to understand FORM as a term."))

(defmethod form->term ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (op-and-args->term (symbolify-here (car list))
			 (cdr list))))

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
  (let ((input-form (let ((*package* (find-package :dialogues)))
		      (read stream nil nil))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro -> (antecdent consequent)
  `(make-implication ,antecdent ,consequent))

(defmacro & (lhs rhs)
  `(make-binary-conjunction ,lhs ,rhs))

(defmacro v (lhs rhs)
  `(make-binary-disjunction ,lhs ,rhs))

(defmacro neg (argument)
  `(negate ,argument))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tautologies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric leftmost-atom (formula))

(defmethod leftmost-atom ((formula (eql top)))
  nil)

(defmethod leftmost-atom ((formula (eql contradiction)))
  nil)

(defmethod leftmost-atom ((formula atomic-formula))
  formula)

(defmethod leftmost-atom ((formula unary-connective-formula))
  (leftmost-atom (argument formula)))

(defmethod leftmost-atom ((formula binary-connective-formula))
  (let ((left (leftmost-atom (lhs formula))))
    (if (null left)
	(leftmost-atom (rhs formula))
	left)))

(defgeneric reduce-top-and-bottom-formula (formula))

(defmethod reduce-top-and-bottom-formula ((formula (eql top)))
  top)

(defmethod reduce-top-and-bottom-formula ((formula (eql contradiction)))
  contradiction)

(defmethod reduce-top-and-bottom-formula ((formula negation))
  (let ((reduced (reduce-top-and-bottom-formula (unnegate formula))))
    (if (eq reduced top)
	contradiction
	top)))

(defmethod reduce-top-and-bottom-formula ((formula binary-conjunction))
  (let ((reduced-lhs (reduce-top-and-bottom-formula (lhs formula))))
    (if (eq reduced-lhs contradiction)
	contradiction
	(reduce-top-and-bottom-formula (rhs formula)))))

(defmethod reduce-top-and-bottom-formula ((formula binary-disjunction))
  (let ((reduced-lhs (reduce-top-and-bottom-formula (lhs formula))))
    (if (eq reduced-lhs top)
	top
	(reduce-top-and-bottom-formula (rhs formula)))))

(defmethod reduce-top-and-bottom-formula ((formula implication))
  (let ((reduced-antecedent (reduce-top-and-bottom-formula (antecedent formula))))
    (if (eq reduced-antecedent contradiction)
	top
	(reduce-top-and-bottom-formula (consequent formula)))))

(defmethod reduce-top-and-bottom-formula ((formula equivalence))
  (let ((reduced-lhs (reduce-top-and-bottom-formula (lhs formula)))
	(reduced-rhs (reduce-top-and-bottom-formula (rhs formula))))
    (if (eq reduced-lhs reduced-rhs)
	top
	contradiction)))

(defun tautology? (formula)
  (let ((first-atom (leftmost-atom formula)))
    (if (null first-atom) ;; only tops and bottoms remain
	(eq (reduce-top-and-bottom-formula formula) top)
	(let ((subst-true (acons first-atom top nil))
	      (subst-false (acons first-atom contradiction nil)))
	  (let ((formula-with-atom-true (apply-substitution subst-true
							    formula
							    :test #'equal-atomic-formulas?))
		(formula-with-atom-false (apply-substitution subst-false
							     formula
							     :test #'equal-atomic-formulas?)))
	    (and (tautology? formula-with-atom-true)
		 (tautology? formula-with-atom-false)))))))

(defgeneric uniquify-atoms (formula)
  (:documentation "Ensure that all the atoms of FORMULA are distinct objects, even if they have the same print name.  (We treat only the propositional case.)"))

(defmethod uniquify-atoms ((atom atomic-formula))
  (make-instance 'atomic-formula
		 :predicate (predicate atom)
		 :args nil))

(defmethod uniquify-atoms ((formula unary-connective-formula))
  (make-instance (class-of formula)
		 :argument (uniquify-atoms (argument formula))))

(defmethod uniquify-atoms ((formula binary-connective-formula))
  (make-instance (class-of formula)
		 :lhs (uniquify-atoms (lhs formula))
		 :rhs (uniquify-atoms (rhs formula))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric formula-< (formula-1 formula-2))

(defmethod formula-< ((formula-1 atomic-formula) (formula-2 atomic-formula))
  (let ((pred-1 (predicate formula-1))
	(pred-2 (predicate formula-2)))
    (lex< (format nil "~a" pred-1)
	  (format nil "~a" pred-2))))

(defmethod formula-< ((formula-1 atomic-formula) (formula-2 t))
  t)

(defmethod formula-< ((formula-1 unary-connective-formula) (formula-2 atomic-formula))
  nil)

(defmethod formula-< ((formula-1 unary-connective-formula) (formula-2 unary-connective-formula))
  (formula-< (argument formula-1)
	     (argument formula-2)))

(defmethod formula-< ((formula-1 unary-connective-formula) (formula-2 binary-connective-formula))
  t)

(defmethod formula-< ((formula-1 binary-connective-formula) (formula-2 atomic-formula))
  nil)

(defmethod formula-< ((formula-1 binary-connective-formula) (formula-2 unary-connective-formula))
  nil)

(defmethod formula-< ((formula-1 binary-connective-formula) (formula-2 binary-connective-formula))
  (or (formula-< (lhs formula-1)
		 (lhs formula-2))
      (formula-< (rhs formula-1)
		 (rhs formula-2))))

(defgeneric appears-in (term thing)
  (:documentation "Does TERM appear in THING?"))

(defmethod appears-in ((term term) (thing atomic-formula))
  (some #'(lambda (x)
            (appears-in term x))
        (arguments thing)))

(defmethod appears-in ((term term) (thing term))
  (or (equal-terms? term thing)
      (some #'(lambda (x)
                (appears-in term x))
            (arguments term))))

(defmethod appears-in ((term term) (thing negation))
  (appears-in term (argument thing)))

(defmethod appears-in ((term term) (thing binary-connective-formula))
  (or (appears-in term (lhs thing))
      (appears-in term (rhs thing))))

(defmethod appears-in ((term term) (thing multiple-arity-connective-formula))
  (some #'(lambda (x)
            (appears-in term x))
        (arguments thing)))

(defmethod appears-in ((term term) (thing generalization))
  (appears-in term (matrix thing)))

;;; formulas.lisp ends here
