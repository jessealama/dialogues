;;; terms.lisp A representation of simple terms

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass atomic-expression (expression)
  ((head
    :initarg :head
    :accessor head
    :initform (error "An atomic expression needs a head.")
    :type string)
   (arguments
    :initarg :arguments
    :accessor arguments
    :initform nil
    :type list)))

(defmethod print-object ((term atomic-expression) stream)
  (with-slots (head arguments)
      term
    (if (null arguments)
	(format stream "~a" head)
	(format stream "~a(~{~a~^,~})" head arguments))))

(defclass term () nil)

(defun term? (thing)
  (typep thing 'term))

(defclass function-term (atomic-expression term)
  nil)

(defgeneric function-symbol (x)
  (:documentation "The function symbol of a function term."))

(defmethod function-symbol ((x t))
  (error "How to extract the function symbol of an object~%~%  ~a~%~%of class~%~~%  ~a~%~%?" x (class-of x)))

(defmethod function-symbol ((x function-term))
  (head x))

(defun make-function-term (function &rest args)
  (make-instance 'function-term
		 :function function
		 :args args))

(defclass variable-term (atomic-expression term)
  nil)

(defun variable-term-p (x)
  (typep x 'variable-term))

(defgeneric equal-variables? (var-1 var-2))

(defmethod equal-variables? ((var-1 variable-term) (var-2 variable-term))
  (string= (head var-1) (head var-2)))

(defgeneric equal-terms? (term-1 term-2))

(defmethod equal-terms? ((v-1 variable-term) (v-2 variable-term))
  (equal-variables? v-1 v-2))

(defmethod equal-terms? ((v variable-term) (f function-term))
  nil)

(defmethod equal-terms? ((f function-term) (v variable-term))
  nil)

(defmethod equal-terms? ((f-1 function-term) (f-2 function-term))
  (when (string= (head f-1) (head f-2))
    (let ((args-1 (arguments f-1))
          (args-2 (arguments f-2)))
      (when (length= args-1 args-2)
        (every #'equal-terms? args-1 args-2)))))

(defun make-variable (symbol-or-string)
  (let ((name (if (symbolp symbol-or-string)
			   (symbol-name symbol-or-string)
			   symbol-or-string)))
    (cond ((string= name "")
	   (error "One cannot make a variable with an empty name"))
	  (t (make-symbol (concatenate 'string "?" name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass formula ()
  nil)

(defun formula-p (x)
  (typep x 'formula))

(defclass atomic-formula (formula atomic-expression)
  nil)

(defclass equation ()
  ((lhs
    :accessor lhs
    :initarg :lhs
    :initform (error "An equation needs a left-hand side."))
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform (error "An equation needs a right-hand side."))))

(defmethod print-object ((x equation) stream)
  (format stream "~a = ~a" (lhs x) (rhs x)))

(defclass disequation ()
  ((lhs
    :accessor lhs
    :initarg :lhs
    :initform (error "A disequation needs a left-hand side."))
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform (error "A disquation needs a right-hand side."))))

(defmethod print-object ((x disequation) stream)
  (format stream "~a != ~a" (lhs x) (rhs x)))

(defclass verum ()
  nil)

(defmethod print-object ((x verum) stream)
  (format stream "$true"))

(defclass falsum ()
  nil)

(defmethod print-object ((x falsum) stream)
  (format stream "$false"))

(defgeneric atomic-formula-p (x)
  (:documentation "Is X an atomic formula?"))

(defmethod atomic-formula-p ((thing t))
  (or (typep thing 'atomic-formula)
      (typep thing 'equation)
      (typep thing 'disequation)
      (typep thing 'verum)
      (typep thing 'falsum)))

(defmethod print-object ((x disequation) stream)
  (format stream "~a != ~a" (lhs x) (rhs x)))

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defclass negation (unary-connective-formula)
  nil)

(defgeneric negation-p (x)
  (:documentation "Is X a negation?"))

(defmethod negation-p ((x t))
  (typep x 'negation))

(defgeneric literal-p (x)
  (:documentation "Is X a literal (atomic formula or negation of an atomic formula)?"))

(defmethod literal-p ((x t))
  (or (atomic-formula-p x)
      (and (negation-p x)
           (atomic-formula-p (argument x)))))

(defclass binary-connective-formula (composite-formula)
  ((lhs :initarg :lhs
	:accessor lhs
	:type formula)
   (rhs :initarg :rhs
	:accessor rhs
	:type formula)))

(defclass binary-conjunction (binary-connective-formula)
  nil)

(defmethod print-object ((x binary-conjunction) stream)
  (format stream "(~a => ~a)" (lhs x) (rhs x)))

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defmethod print-object ((x binary-disjunction) stream)
  (format stream "(~a | ~a)" (lhs x) (rhs x)))

(defclass implication (binary-connective-formula)
  nil)

(defun implication-p (x)
  (typep x 'implication))

(defmethod print-object ((x implication) stream)
  (format stream "(~a => ~a)" (lhs x) (rhs x)))

(defclass reverse-implication (binary-connective-formula)
  nil)

(defmethod print-object ((x reverse-implication) stream)
  (format stream "(~a <= ~a)" (lhs x) (rhs x)))

(defclass equivalence (binary-connective-formula)
  nil)

(defmethod print-object ((x equivalence) stream)
  (format stream "(~a <=> ~a)" (lhs x) (rhs x)))

;; quantifiers

(defclass generalization (composite-formula)
  ((bindings :initarg :bindings
	     :accessor bindings
	     :type list)
   (matrix :initarg :matrix
	   :accessor matrix
	   :type formula)))

(defclass universal-generalization (generalization)
  nil)

(defmethod print-object ((uni-gen universal-generalization) stream)
  (format stream "(! [~{~a~^,~}] : ~a)" (bindings uni-gen) (matrix uni-gen)))

(defclass existential-generalization (generalization)
  nil)

(defmethod print-object ((exi-gen existential-generalization) stream)
  (format stream "(? [~{~a~^,~}] : ~a)" (bindings exi-gen) (matrix exi-gen)))

(defgeneric make-atomic-formula (predicate &rest arguments))

(defmethod make-atomic-formula ((predicate symbol) &rest arguments)
    (make-instance 'atomic-formula
                   :head (symbol-name predicate)
                   :arguments arguments))

(defmethod make-atomic-formula ((predicate string) &rest arguments)
  (make-instance 'atomic-formula
                 :head predicate
                 :arguments arguments))

(defun make-equation (lhs rhs)
  (make-atomic-formula '= lhs rhs))

(defclass composite-formula (formula)
  nil)

(defun composite-formula-p (x)
  "Determine whether X is non-atomic.

Unlike other predicates such as BINARY-DISJUNCTION? and
UNIVERSAL-GENERALIZATION?, this predicate does not merely test whether
the direct class of its argument is COMPOSITE-FORMULA.  The class
COMPOSITE-FORMULA is defined only to provide a common superclass for
further subclasses, such as BINARY-DISJUNCTION and
UNIVERSAL-GENERALIZATION, that is intended to be disjoint from the
class ATOMIC-FORMULA.  This function expresses that disjointedness."
  (and (formula-p x)
       (not (atomic-formula-p x))))

(defun binary-connective-formula? (thing)
  (typep thing 'binary-connective-formula))

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
      (make-instance 'verum)))

(defun binary-disjunction->multiple-arity-disjunction (binary-disjunction)
  (make-instance 'multiple-arity-disjunction
		 :items (list (lhs binary-disjunction)
			      (rhs binary-disjunction))))

(defun multiple-arity-disjunction->binary-disjunction (multiple-arity-disjunction)
  (let ((disjuncts (items multiple-arity-disjunction)))
    (if (null disjuncts)
	(make-instance 'binary-disjunction
		       :lhs (make-instance 'verum)
		       :rhs (make-instance 'verum))
	(if (null (cdr disjuncts))
	    (make-instance 'binary-disjunction
			   :lhs (first disjuncts)
			   :rhs (make-instance 'contradiction))
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
      (make-instance 'falsum)))

(defun binary-conjunction->multiple-arity-conjunction (binary-conjunction)
  (make-instance 'multiple-arity-conjunction
		 :items (list (lhs binary-conjunction)
			      (rhs binary-conjunction))))

(defun multiple-arity-conjunction->binary-conjunction (multiple-arity-conjunction)
  (let ((conjuncts (items multiple-arity-conjunction)))
    (if (null conjuncts)
	(make-binary-conjunction (make-instance 'falsum)
                                 (make-instance 'falsum))
	(if (null (cdr conjuncts))
	    (make-instance 'binary-conjunction
			   :lhs (first conjuncts)
			   :rhs (make-instance 'verum))
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
		 :bindings (list var)
		 :matrix formula))

(defun make-existential (var formula)
  (make-instance 'existential-generalization
		 :bindings (list var)
		 :matrix formula))

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

(defgeneric subst-term-for-var-in-term (term var target-term))

(defmethod subst-term-for-var-in-term ((term term)
				       (var variable-term)
				       (target-term variable-term))
  (if (equal-variables? var target-term)
      term
      target-term))

(defmethod subst-term-for-var-in-term ((term term)
				       (var variable-term)
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
  (let ((pred (head formula))
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
  (let ((bindings (bindings formula))
        (matrix (matrix formula)))
    (if (member variable bindings :test #'equal-variables?)
        (let ((other-bindings (remove-if-not #'(lambda (x)
                                                 (equal-variables? x variable))
                                             bindings))
              (subst (subst-term-for-var-in-term term variable matrix)))
          (if (null other-bindings)
              subst
              (make-instance (class-of formula)
                             :bindings other-bindings
                             :matrix subst)))
        (make-instance (class-of formula)
                       :bindings bindings
                       :matrix (instantiate term variable matrix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sundry formula-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equal-formulas? (formula-1 formula-2))

(defmethod equal-formulas? ((form-1 atomic-formula) (form-2 atomic-formula))
  (and (string= (head form-1)
                (head form-2))
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
  (and (atomic-formula-p formula-1)
       (atomic-formula-p formula-2)
       (equal-formulas? formula-1 formula-2)))

(defgeneric contains-contradiction-p (x)
  (:documentation "Is a contradiction (bottom) found anywhere inside X?"))

(defmethod contains-contradiction-p ((x verum))
  nil)

(defmethod contains-contradiction-p ((x falsum))
  t)

(defmethod contains-contradiction-p ((x atomic-formula))
  nil)

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
  nil)

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

(defun read-formula (&optional (stream *standard-input*))
  (declare (ignore stream))
  (error "READ-FORMULA is dead."))

(defun parse-formula (str)
  (with-input-from-string (s str)
    (read-formula s)))

(defun read-new-formula ()
  (format t "Enter a new formula: ")
  (multiple-value-list (read-formula)))

(defun read-atomic-formula ()
  (let (response)
    (until (atomic-formula-p response)
      (read-formula))
    response))

(defun read-composite-formula (&optional (stream *standard-input*))
  (declare (ignore stream))
  (error "READ-COMPOSITE-FORMULA is dead."))

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

(defgeneric uniquify-atoms (formula)
  (:documentation "Ensure that all the atoms of FORMULA are distinct objects, even if they have the same print name.  (We treat only the propositional case.)"))

(defmethod uniquify-atoms ((atom atomic-formula))
  (make-instance 'atomic-formula
		 :head (head atom)
		 :arguments nil))

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
  (let ((pred-1 (head formula-1))
	(pred-2 (head formula-2)))
    (lex< pred-1 pred-2)))

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

(defclass symbolic-attack ()
  ((name
    :initarg :name
    :type string
    :accessor name
    :initform (error "A symbol attack needs a name."))))

(defmethod print-object ((sa symbolic-attack) stream)
  (format stream "~:(~a~)" (name sa)))

(defclass which-instance-attack (symbolic-attack)
  ((instance
    :initarg :instance
    :accessor instance
    :type (or null term)
    :initform nil)))

(defmethod print-object ((sa which-instance-attack) stream)
  (let ((instance (instance sa)))
    (if (null instance)
        (format stream "?")
        (format stream "?-~a" instance))))

(defun which-instance-attack-p (x)
  (eql (class-of x) 'which-instance-attack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard symbolic attacks for propositional and first-order languges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *attack-left-conjunct*
  (make-instance 'symbolic-attack
                 :name "attack-left-conjunct"))

(defparameter *attack-right-conjunct*
  (make-instance 'symbolic-attack
                 :name "attack-right-conjunct"))

(defparameter *which-disjunct?*
  (make-instance 'symbolic-attack
                 :name "which-disjunct?"))

(defparameter *propositional-symbolic-attacks*
  (list *attack-left-conjunct*
	*attack-right-conjunct*
	*which-disjunct?*)
  "The three symbolic attacks that are permitted in propositional
  dialogue games:

- attack the left conjunct,
- attack the right conjunct, and
- attack a disjunction by requesting one of the disjuncts.")

(defun symbolic-attack-p (obj)
  (typep obj 'symbolic-attack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equal-statements? (statement-1 statement-2))

(defmethod equal-statements? ((form-1 formula) (form-2 formula))
  (equal-formulas? form-1 form-2))

(defmethod equal-statements? ((form formula) (sa symbolic-attack))
  nil)

(defmethod equal-statements? ((form formula) (term term))
  nil)

(defmethod equal-statements? ((sa symbolic-attack) (form formula))
  nil)

(defmethod equal-statements? ((sa-1 symbolic-attack) (sa-2 symbolic-attack))
  (eq sa-1 sa-2))

(defmethod equal-statements? ((sa symbolic-attack) (term term))
  nil)

(defmethod equal-statements? ((term term) (formula formula))
  nil)

(defmethod equal-statements? ((term term) (sa symbolic-attack))
  nil)

(defmethod equal-statements? ((term-1 term) (term-2 term))
  (equal-terms? term-1 term-2))

(defun non-symbolic-attack-term? (obj)
  "Determine whether OBJ is a term different from the symbolic
attacks which, being symbols, do qualify as terms."
  (and (not (symbolic-attack-p obj))
       (term? obj)))

(defun non-symbolic-attack-formula? (obj)
  "Determine whether OBJ is a formula different from the symbolic
  attacks which, being simply lisp symbols, do qualify as [atomic]
  formulas)."
  (and (not (symbolic-attack-p obj))
       (formula-p obj)))

(defgeneric statement-< (statement-1 statement-2))

(defmethod statement-< ((statement-1 symbolic-attack) (statement-2 symbolic-attack))
  (or (eq statement-1 *attack-left-conjunct*)
      (and (eq statement-1 *attack-right-conjunct*)
	   (not (eq statement-2 *attack-left-conjunct*)))))

(defmethod statement-< ((statement-1 symbolic-attack) (statement-2 t))
  nil)

(defmethod statement-< ((statement-1 formula) (statement-2 symbolic-attack))
  nil)

(defmethod statement-< ((statement-1 formula) (statement-2 formula))
  (formula-< statement-1 statement-2))
