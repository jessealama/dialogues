;;; terms.lisp A representation of simple terms

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass expression ()
  nil)

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

(defun term-p (thing)
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

(defgeneric make-variable (x)
  (:documentation "Make a variable named 'X'."))

(defmethod make-variable ((x string))
  (make-instance 'variable-term
                 :head x))

(defmethod make-variable ((x symbol))
  (make-variable (symbol-name x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass formula (expression)
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

(defun verum-p (x)
  "Is X verum?"
  (typep x 'verum))

(defclass falsum ()
  nil)

(defmethod print-object ((x falsum) stream)
  (format stream "$false"))

(defun falsum-p (x)
  "Is X falsum?"
  (typep x 'falsum))

(defgeneric atomic-formula-p (x)
  (:documentation "Is X an atomic formula?"))

(defmethod atomic-formula-p ((x t))
  nil)

(defun non-atomic-formula-p (x)
  (when (formula-p x)
    (not (atomic-formula-p x))))

(defmethod atomic-formula-p ((thing formula))
  (or (typep thing 'atomic-formula)
      (typep thing 'equation)
      (typep thing 'disequation)
      (typep thing 'verum)
      (typep thing 'falsum)))

(defmethod print-object ((x disequation) stream)
  (format stream "~a != ~a" (lhs x) (rhs x)))

(defclass composite-formula (formula)
  nil)

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defclass negation (unary-connective-formula)
  nil)

(defmethod print-object ((x negation) stream)
  (format stream "~~~a" (argument x)))

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
  (format stream "(~a & ~a)" (lhs x) (rhs x)))

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defmethod print-object ((x binary-disjunction) stream)
  (format stream "(~a | ~a)" (lhs x) (rhs x)))

(defun binary-disjunction-p (x)
  (typep x 'binary-disjunction))

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

(defgeneric generalization-p (thing)
  (:documentation "Is THING a generalization?"))

(defmethod generalization-p ((thing t))
  nil)

(defmethod generalization-p ((thing expression))
  (typep thing 'generalization))

(defclass universal-generalization (generalization)
  nil)

(defmethod print-object ((uni-gen universal-generalization) stream)
  (format stream "(! [~{~a~^,~}] : ~a)" (bindings uni-gen) (matrix uni-gen)))

(defgeneric universal-generalization-p (x)
  (:documentation "Is X a universal generalization (forall)?"))

(defmethod universal-generalization-p ((x t))
  nil)

(defmethod universal-generalization-p ((x expression))
  (typep x 'universal-generalization))

(defclass existential-generalization (generalization)
  nil)

(defmethod print-object ((exi-gen existential-generalization) stream)
  (format stream "(? [~{~a~^,~}] : ~a)" (bindings exi-gen) (matrix exi-gen)))

(defgeneric existential-generalization-p (x)
  (:documentation "Is X an existential generalization (there exists)?"))

(defmethod existential-generalization-p ((x t))
  nil)

(defmethod existential-generalization-p ((x expression))
  (typep x 'existential-generalization))

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
  ((arguments :initarg :arguments
	  :accessor arguments
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

(defmethod print-object ((x multiple-arity-disjunction) stream)
  (let ((args (arguments x)))
    (cond ((null args)
           (error "A multiple arity disjunction has zero arguments."))
          ((length= args 1)
           (format stream "~a" (first args)))
          (t
           (loop
              :initially (format stream "(~a" (first args))
              :for arg :in (rest args)
              :do (format stream " | ~a" arg)
              :finally (format stream ")"))))))

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
			     :arguments disjuncts)
	      (car disjuncts)))
      (make-instance 'verum)))

(defun binary-disjunction->multiple-arity-disjunction (binary-disjunction)
  (make-instance 'multiple-arity-disjunction
		 :arguments (list (lhs binary-disjunction)
			      (rhs binary-disjunction))))

(defun multiple-arity-disjunction->binary-disjunction (multiple-arity-disjunction)
  (let ((disjuncts (arguments multiple-arity-disjunction)))
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

(defun binary-conjunction-p (thing)
  (typep thing 'binary-conjunction))

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defun multiple-arity-conjunction? (thing)
  (eql (class-of thing) 'multiple-arity-conjunction))

(defmethod print-object ((x multiple-arity-conjunction) stream)
  (let ((args (arguments x)))
    (cond ((null args)
           (error "A multiple arity conjunction has zero arguments."))
          ((length= args 1)
           (format stream "~a" (first args)))
          (t
           (loop
              :initially (format stream "(~a" (first args))
              :for arg :in (rest args)
              :do (format stream " & ~a" arg)
              :finally (format stream ")"))))))

(defun make-binary-conjunction (lhs rhs)
  (make-instance 'binary-conjunction
		 :lhs lhs
		 :rhs rhs))

(defun make-multiple-arity-conjunction (&rest conjuncts)
  (if conjuncts
      (if (rest conjuncts)
	  (if (rest (rest conjuncts))
	      (make-instance 'multiple-arity-conjunction
			     :arguments conjuncts))
	  (first conjuncts))
      (make-instance 'falsum)))

(defun binary-conjunction->multiple-arity-conjunction (binary-conjunction)
  (make-instance 'multiple-arity-conjunction
		 :arguments (list (lhs binary-conjunction)
			      (rhs binary-conjunction))))

(defun multiple-arity-conjunction->binary-conjunction (multiple-arity-conjunction)
  (let ((conjuncts (arguments multiple-arity-conjunction)))
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
  (let ((arguments (arguments formula)))
    (append arguments
	    (mapcar #'proper-subformulas-1 arguments))))

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
		 :arguments (mapcar #'(lambda (item)
				    (instantiate term variable item))
				(arguments formula))))

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

(defmethod equal-formulas? ((formula-1 t) (formula-2 t))
  "By default, if no other generic function applies, the answer is no."
  nil)

(defmethod equal-formulas? ((form-1 atomic-formula) (form-2 atomic-formula))
  (and (string= (head form-1)
                (head form-2))
       (every-pair #'(lambda (term-1 term-2)
		       (equal-terms? term-1 term-2))
		   (arguments form-1)
		   (arguments form-2))))

(defmethod equal-formulas? ((form-1 negation) (form-2 negation))
  (equal-formulas? (unnegate form-1)
		   (unnegate form-2)))

(defmethod equal-formulas? ((form-1 binary-connective-formula)
                            (form-2 binary-connective-formula))
  (and (eql (class-of form-1) (class-of form-2))
       (equal-formulas? (lhs form-1)
			(lhs form-2))
       (equal-formulas? (rhs form-1)
			(rhs form-2))))

(defmethod equal-formulas? ((form-1 multiple-arity-connective-formula)
                            (form-2 multiple-arity-connective-formula))
  (when (eql (class-of form-1) (class-of form-2))
    (let ((args-1 (arguments form-1))
          (args-2 (arguments form-2)))
      (when (length= args-1 args-2)
        (every-pair #'equal-formulas? args-1 args-2)))))

(defmethod equal-formulas? ((form-1 generalization)
                            (form-2 generalization))
  (let ((bindings-1 (bindings form-1))
        (bindings-2 (bindings form-2)))
    (when (subsetp bindings-1 bindings-2 :test #'equal-variables?)
      (when (subsetp bindings-2 bindings-1 :test #'equal-variables?)
        (equal-formulas? (matrix form-1) (matrix form-2))))))

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

(defgeneric contains-equation-p (x)
  (:documentation "Is an equation found anywhere inside X?"))

(defmethod contains-equation-p ((x equation))
  t)

(defmethod contains-equation-p ((x disequation))
  t)

(defmethod contains-equation-p ((x atomic-formula))
  nil)

(defmethod contains-equation-p ((x unary-connective-formula))
  (contains-equation-p (argument x)))

(defmethod contains-equation-p ((x binary-connective-formula))
  (or (contains-equation-p (lhs x))
      (contains-equation-p (rhs x))))

(defmethod contains-equation-p ((x generalization))
  (contains-equation-p (matrix x)))

(defmethod contains-equation-p ((x multiple-arity-connective-formula))
  (some #'contains-equation-p (arguments x)))

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

(defclass symbolic-attack (expression)
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

(defgeneric which-instance-attack-p (x)
  (:documentation "Is X a which-instance attack?"))

(defmethod which-instance-attack-p ((x t))
  nil)

(defmethod which-instance-attack-p ((x expression))
  (typep x 'which-instance-attack))

(defgeneric equal-symbolic-attacks? (sa-1 sa-2)
  (:documentation "Are symbolic attacks SA-1 and SA-2 equal?  (They can be equal without being identical objects.)"))

(defmethod equal-symbolic-attacks? ((sa-1 symbolic-attack) (sa-2 which-instance-attack))
  nil)

(defmethod equal-symbolic-attacks? ((sa-1 which-instance-attack) (sa-2 symbolic-attack))
  nil)

(defmethod equal-symbolic-attacks? ((sa-1 which-instance-attack)
                                    (sa-2 which-instance-attack))
  (let ((i-1 (instance sa-1))
        (i-2 (instance sa-2)))
    (or (and (null i-1)
             (null i-2))
        (and (term-p i-1)
             (term-p i-2)
             (equal-terms? i-1 i-2)))))

(defmethod equal-symbolic-attacks? ((sa-1 symbolic-attack)
                                    (sa-2 symbolic-attack))
  (eq sa-1 sa-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard symbolic attacks for propositional and first-order languges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *attack-atom*
  (make-instance 'symbolic-attack
                 :name "attack-atom"))

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
       (term-p obj)))

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

(defgeneric equivalence->conjunction (thing)
  (:documentation "Rewrite equivalences in THING as a conjunction of implications."))

(defmethod equivalence->conjunction ((thing equivalence))
  (let ((l (equivalence->conjunction (lhs thing)))
        (r (equivalence->conjunction (rhs thing))))
    (make-binary-conjunction (make-implication l r)
                             (make-implication r l))))

(defmethod equivalence->conjunction ((thing atomic-formula))
  thing)

(defmethod equivalence->conjunction ((x binary-connective-formula))
  (make-instance (class-of x)
                 :lhs (equivalence->conjunction (lhs x))
                 :rhs (equivalence->conjunction (rhs x))))

(defmethod equivalence->conjunction ((thing negation))
  (negate (equivalence->conjunction (unnegate thing))))

(defmethod equivalence->conjunction ((x multiple-arity-connective-formula))
  (make-instance (class-of x)
                 :arguments (mapcar #'equivalence->conjunction
                                    (arguments x))))

(defmethod equivalence->conjunction ((x generalization))
  (make-instance (class-of x)
                 :bindings (bindings x)
                 :matrix (equivalence->conjunction (matrix x))))

(defgeneric binarize (x)
  (:documentation "Replace occurrences of higher-arity connectives by their binary counterparts."))

(defmethod binarize ((thing atomic-formula))
  thing)

(defmethod binarize ((x binary-connective-formula))
  (make-instance (class-of x)
                 :lhs (binarize (lhs x))
                 :rhs (binarize (rhs x))))

(defmethod binarize ((thing negation))
  (negate (binarize (unnegate thing))))

(defmethod binarize ((x multiple-arity-disjunction))
  (let ((a (arguments x)))
    (cond ((null a)
           (error "Empty list of arguments in a multiple-arity disjunction."))
          ((length= a 1)
           (binarize (first a)))
          ((length= a 2)
           (make-binary-disjunction (first a) (second a)))
          (t
           (let ((x (first a))
                 (y (make-instance 'multiple-arity-disjunction
                                   :arguments (rest a))))
             (make-binary-disjunction x (binarize y)))))))

(defmethod binarize ((x multiple-arity-conjunction))
  (let ((a (arguments x)))
    (cond ((null a)
           (error "Empty list of arguments in a multiple-arity conjunction."))
          ((length= a 1)
           (binarize (first a)))
          ((length= a 2)
           (make-binary-conjunction (first a) (second a)))
          (t
           (let ((x (first a))
                 (y (make-instance 'multiple-arity-conjunction
                                   :arguments (rest a))))
             (make-binary-conjunction x (binarize y)))))))

(defmethod binarize ((x generalization))
  (make-instance (class-of x)
                 :bindings (bindings x)
                 :matrix (binarize (matrix x))))

(defgeneric instantiate (statement term variable)
  (:documentation "Plug TERM in for VARIABLE in STATEMENT."))

(defmethod instantiate ((statements list) term variable)
  (mapcar #'(lambda (x)
              (instantiate x term variable))
          statements))

(defmethod instantiate ((statement atomic-formula) term variable)
  (make-instance 'atomic-formula
                 :head (head statement)
                 :arguments (instantiate (arguments statement) term variable)))

(defmethod instantiate ((statement variable-term) term variable)
  (if (equal-variables? statement variable)
      term
      statement))

(defmethod instantiate ((statement function-term) term variable)
  (make-instance 'function-term
                 :head (head statement)
                 :arguments (instantiate (arguments statement) term variable)))

(defmethod instantiate ((statement binary-connective-formula) term variable)
  (make-instance (class-of statement)
                 :lhs (instantiate (lhs statement) term variable)
                 :rhs (instantiate (rhs statement) term variable)))

(defmethod instantiate ((statement unary-connective-formula) term variable)
  (make-instance (class-of statement)
                :argument (instantiate (argument statement) term variable)))

(defmethod instantiate ((statement multiple-arity-connective-formula) term variable)
  (make-instance (class-of statement)
                 :arguments (instantiate (arguments statement) term variable)))

(defmethod instantiate ((statement generalization) term variable)
  (let ((remaining-bindings (remove variable (bindings statement) :test #'equal-variables?)))
    (if (null remaining-bindings)
        (instantiate (matrix statement) term variable)
        (make-instance (class-of statement)
                       :bindings remaining-bindings
                       :matrix (instantiate (matrix statement) term variable)))))

(defgeneric terms-in (x)
  (:documentation "The (distinct) terms appearing in X."))

(defmethod terms-in ((x list))
  (remove-duplicates (reduce #'append (mapcar #'terms-in x))
                     :test #'equal-terms?))

(defmethod terms-in ((x variable-term))
  (list x))

(defmethod terms-in ((x function-term))
  (terms-in (arguments x)))

(defmethod terms-in ((x atomic-formula))
  (terms-in (arguments x)))

(defmethod terms-in ((x unary-connective-formula))
  (terms-in (argument x)))

(defmethod terms-in ((x binary-connective-formula))
  (remove-duplicates (append (terms-in (lhs x))
                             (terms-in (rhs x)))
                     :test #'equal-terms?))

(defmethod terms-in ((x multiple-arity-connective-formula))
  (terms-in (arguments x)))

(defmethod terms-in ((x generalization))
  (remove-duplicates (append (bindings x) (terms-in (matrix x)))
                     :test #'equal-terms?))

(defun variables-in (x)
  (remove-if-not #'variable-term-p (terms-in x)))

(defun non-variable-terms-in (x)
  "The non-variable terms appearing in X."
  (remove-if #'variable-term-p (terms-in x)))

(defparameter *fresh-variable-prefix* "X")

(defun fresh-variable (x)
  "A variable that appears nowhere in X."
  (loop
     :with vars = (variables-in x)
     :with num-vars = (length vars)
     :for i :from 1 :upto num-vars
     :for candidate-name = (format nil "~a~d" *fresh-variable-prefix* i)
     :for candidate = (make-variable candidate-name)
     :do
     (unless (some #'(lambda (v) (equal-variables? candidate v)) vars)
       (return candidate))
     :finally (return (make-variable (format nil "~a~d" *fresh-variable-prefix* (1+ num-vars))))))

(defgeneric term-depth (x)
  (:documentation "The term depth of X."))

(defmethod term-depth ((x variable-term))
  0)

(defmethod term-depth ((x function-term))
  (1+ (apply #'max (mapcar #'term-depth (arguments x)))))

(defgeneric occurs-freely (term thing)
  (:documentation "Does TERM occur freely in THING?"))

(defmethod occurs-freely (term (thing list))
  (some #'(lambda (x)
            (occurs-freely term x))
        thing))

(defmethod occurs-freely ((term variable-term) (thing variable-term))
  (equal-variables? term thing))

(defmethod occurs-freely ((term t) (thing variable-term))
  nil)

(defmethod occurs-freely (term (thing atomic-expression))
  (some #'(lambda (x)
            (equal-terms? term x))
        (arguments thing)))

(defmethod occurs-freely (term (thing unary-connective-formula))
  (occurs-freely term (argument thing)))

(defmethod occurs-freely (term (thing binary-connective-formula))
  (or (occurs-freely term (lhs thing))
      (occurs-freely term (rhs thing))))

(defmethod occurs-freely (term (thing multiple-arity-connective-formula))
  (some #'(lambda (x)
            (occurs-freely term x))
        (arguments thing)))

(defmethod occurs-freely ((term variable-term) (thing generalization))
  (unless (member term (bindings thing) :test #'equal-variables?)
    (occurs-freely term (matrix thing))))

(defmethod occurs-freely ((term t) (thing t))
  (error "How to determine whether~%~%  ~a~%~%occurs freely in~%~%  ~a~%~%?" term thing))

(defgeneric free-variables (x)
  (:documentation "The set of variables occurring freely in X."))

(defmethod free-variables ((thing list))
  (remove-duplicates (reduce #'append (mapcar #'free-variables thing))
                     :test #'equal-variables?))

(defmethod free-variables ((thing variable-term))
  (list thing))

(defmethod free-variables ((thing atomic-expression))
  (free-variables (arguments thing)))

(defmethod free-variables ((thing symbolic-attack))
  nil)

(defmethod free-variables ((thing unary-connective-formula))
  (free-variables (argument thing)))

(defmethod free-variables ((thing binary-connective-formula))
  (remove-duplicates (append (free-variables (lhs thing))
                             (free-variables (rhs thing)))
                     :test #'equal-variables?))

(defmethod free-variables ((thing multiple-arity-connective-formula))
  (remove-duplicates (reduce #'append (mapcar #'free-variables (arguments thing)))))

(defmethod free-variables ((thing generalization))
  (let ((free-in-matrix (free-variables (matrix thing)))
        (bindings (bindings thing)))
    (remove-if #'(lambda (x)
                   (member x bindings :test #'equal-variables?))
               free-in-matrix)))

(defmethod free-variables ((thing t))
  (error "How to determine the set of free variables of~%~%  ~a~%~%?" thing))

(defgeneric equal-expressions? (expression-1 expression-2)
  (:documentation "Are EXPRESSION-1 and EXPRESSION-2 equal?  (We are not testing identity, but equality.)"))

(defmethod equal-expressions? ((expression-1 t) (expression-2 t))
  nil)

(defmethod equal-expressions? ((expression-1 formula) (expression-2 formula))
  (equal-formulas? expression-1 expression-2))

(defmethod equal-expressions? ((expression-1 term) (expression-2 term))
  (equal-terms? expression-1 expression-2))

(defmethod equal-expressions? ((e-1 symbolic-attack) (e-2 symbolic-attack))
  (equal-symbolic-attacks? e-1 e-2))
