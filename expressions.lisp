
(in-package :dialogues)

(defclass expression ()
  nil)

(defclass atomic-expression (expression)
  ((head
    :initarg :head
    :accessor head
    :initform (error "An atomic expression needs a head.")
    :type symbol)
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

(defclass general-list ()
  ((terms
    :type list
    :accessor terms
    :initarg :terms
    :initform nil)))

(defmethod print-object ((l general-list) stream)
  (format stream "[~{~a~^,~}]" (terms l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass term (expression) nil)

(defun term? (thing)
  (typep thing 'term))

(defclass function-term (atomic-expression term)
  nil)

(defun make-function-term (function &rest args)
  (make-instance 'function-term
		 :function function
		 :args args))

(defclass variable-term (atomic-expression term)
  nil)

(defun variable-term-p (x)
  (typep x 'variable-term))

(defmethod print-object ((var variable-term) stream)
  (format stream "~a" (head var)))

(defun variable? (thing)
  (typep thing 'variable-term))

(defgeneric form->term (form)
  (:documentation "Attempt to understand FORM as a term."))

(defmethod form->term ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (op-and-args->term (symbolify-here (car list))
			 (cdr list))))

(defmethod form->term ((term string))
  (list term))

(defmethod form->term ((sym symbol))
  (let ((name (symbol-name sym)))
    (if (empty-string? name)
	(error 'parse-form-empty-string-supplied)
	(let ((first-char (char name 0)))
	  (if (char= first-char #\?)
	      (make-instance 'variable-term
			     :name (subseq name 1))
	      (make-function-term name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *all-symbol* (symbolify-here "all"))
(defparameter *exists-symbol* (symbolify-here "exists"))
(defparameter *or-symbol* (symbolify-here "or"))
(defparameter *and-symbol* (symbolify-here "and"))
(defparameter *negation-symbol* (symbolify-here "not"))
(defparameter *implication-symbol* (symbolify-here "implies"))
(defparameter *equivalence-symbol* (symbolify-here "iff"))
(defparameter *nonequivalence-symbol* (symbolify-here "xor"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass formula ()
  nil)

(defun formula? (thing)
  (typep thing 'formula))

(defclass atomic-formula (formula)
  ((predicate
    :initarg :predicate
    :accessor predicate)
   (arguments
    :initarg :arguments
    :accessor arguments
    :type list)))

(defgeneric atomic-formula-p (x))

(defmethod atomic-formula-p ((x t))
  nil)

(defmethod atomic-formula-p ((x atomic-formula))
  t)

(defun atomic-formula? (thing)
  (typep thing 'atomic-formula))

(defparameter *nullary-true*
  (make-instance 'atomic-formula
		 :predicate (intern "true")
		 :arguments nil))

(defparameter *nullary-false*
  (make-instance 'atomic-formula
		 :predicate (intern "false")
		 :arguments nil))

(defclass equation (atomic-formula)
  ((lhs
    :accessor lhs
    :initarg :lhs
    :initform (error "An equation needs a left-hand side."))
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform (error "An equation needs a right-hand side."))))

(defmethod initialize-instance :after ((x equation) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (predicate x) (intern "=" :dialogues)
	(arguments x) (list (lhs x) (rhs x)))
  x)

(defclass disequation (atomic-formula)
  ((lhs
    :accessor lhs
    :initarg :lhs
    :initform (error "A disequation needs a left-hand side."))
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform (error "A disquation needs a right-hand side."))))

(defmethod initialize-instance :after ((x disequation) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (arguments x)
	(list (lhs x) (rhs x)))
  (setf (predicate x) (intern "!="))
  x)

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defclass negation (unary-connective-formula)
  nil)

(defgeneric literal-p (x))

(defmethod literal-p ((x t))
  nil)

(defmethod literal-p ((x negation))
  (atomic-formula-p (argument x)))

(defmethod literal-p ((x atomic-formula))
  t)

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

(defclass reverse-implication (binary-connective-formula)
  nil)

(defclass equivalence (binary-connective-formula)
  nil)

(defclass nonequivalence (binary-connective-formula)
  nil)

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

(defclass existential-generalization (generalization)
  nil)

(defun equation? (formula)
  (when (atomic-formula? formula)
    (let ((pred (predicate formula)))
      (string= (symbol-name pred) "="))))

(defmethod print-object ((atom atomic-formula) stream)
  (let ((pred (predicate atom))
	(args (arguments atom)))
    (if (null args)
	(cond ((string= (stringify pred) "true")
	       (format stream "$true"))
	      ((string= (stringify pred) "false")
	       (format stream "$false"))
	      (t
	       (format stream "~a" pred)))
	(format stream "~a(~{~a~^,~})" pred args))))

(defmethod print-object ((x disequation) stream)
  (with-slots (lhs rhs)
      x
    (format stream "~a != ~a" lhs rhs)))

(defmethod print-object ((x equation) stream)
  (with-slots (lhs rhs)
      x
    (format stream "~a = ~a" lhs rhs)))

(defgeneric render-plainly (statement))

(defgeneric render-fancily (statement))

(defmethod render-plainly ((statement term))
  (let ((func-sym (head statement))
	(args (arguments statement)))
    (if (null args)
	(format nil "~A" func-sym)
	(if (null (cdr args))
	    (format nil "~A(~A)"
		    func-sym
		    (render-plainly (car args)))
	    (concatenate 'string
			(format nil "~A" func-sym)
			"("
			(render-plainly (car args))
			(apply #'concatenate 'string
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
	       (render-plainly (bindings gen))
	       "["
	       (render-plainly (matrix gen))
	       "]"))

(defmethod render-fancily :around ((gen generalization))
  (format nil "~a~a[~a]"
	  (call-next-method)
	  (render-fancily (bindings gen))
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

(defmethod render-plainly ((formula reverse-implication))
  "<--")

(defmethod render-fancily ((formula implication))
  "→")

(defmethod render-plainly ((formula equivalence))
  "<-->")

(defmethod render-fancily ((formula equivalence))
  "↔")

(defmethod render-plainly ((formula nonequivalence))
  "<%~>")

(defmethod render-fancily ((formula nonequivalence))
  "↭")

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
	    (concatenate 'string
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

(defmethod make-atomic-formula ((predicate symbol) &rest arguments)
  (make-instance 'atomic-formula
		 :predicate predicate
		 :arguments (mapcar #'form->term arguments)))

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

(defmethod print-object :around ((formula binary-connective-formula) stream)
  (format stream "(~A " (lhs formula))
  (call-next-method)
  (format stream " ~A)" (rhs formula)))

(defmethod print-object :around ((formula unary-connective-formula) stream)
  (call-next-method)
  (format stream "~A" (argument formula)))

(defgeneric unnegate (formula))

(defmethod unnegate ((negation negation))
  (argument negation))

(defun negation? (thing)
  (typep thing 'negation))

(defmethod print-object ((neg negation) stream)
  (format stream "~a" #\~))

(defgeneric negate (thing))

(defmethod negate ((formula formula))
  (make-instance 'negation :argument formula))

(defmethod negate ((x null))
  nil)

(defmethod negate ((l list))
  (mapcar #'negate l))

(defclass multiple-arity-connective-formula (composite-formula)
  ((items :initarg :items
	  :accessor items
	  :type list)))

(defgeneric connective-unit (multiple-arity-connective-formula))

(defgeneric squeeze-quantifiers (generalization))

(defmethod squeeze-quantifiers ((x atomic-formula))
  x)

(defmethod squeeze-quantifiers ((x negation))
  (make-instance 'negation
		 :argument (squeeze-quantifiers (argument x))))

(defmethod squeeze-quantifiers ((x binary-connective-formula))
  (make-instance (class-of x)
		 :lhs (squeeze-quantifiers (lhs x))
		 :rhs (squeeze-quantifiers (rhs x))))

(defmethod squeeze-quantifiers ((x multiple-arity-connective-formula))
  (make-instance (class-of x)
		 :items (mapcar #'squeeze-quantifiers (items x))))

(defmethod squeeze-quantifiers ((x generalization))
  (if (eql (class-of x)
	   (class-of (matrix x)))
      (squeeze-quantifiers (make-instance (class-of x)
					  :bindings (append (bindings x)
							    (bindings (matrix x)))
					  :matrix (matrix (matrix x))))
      x))

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

(defun implication? (thing)
  (typep thing 'implication))

(defmethod print-object ((implication implication) stream)
  (format stream "=>"))

(defmethod print-object ((implication reverse-implication) stream)
  (format stream "<="))

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

(defmethod print-object ((equiv equivalence) stream)
  (format stream "<=>"))

(defun make-equivalence (lhs rhs)
  (make-instance 'equivalence
		 :lhs lhs
		 :rhs rhs))

(defun nonequivalence? (thing)
  (typep thing 'nonequivalence))

(defmethod print-object ((equiv nonequivalence) stream)
  (format stream "<~a>" #\~))

(defun make-nonequivalence (lhs rhs)
  (make-instance 'nonequivalence
		 :lhs lhs
		 :rhs rhs))

;;; disjunctions

(defun binary-disjunction? (thing)
  (typep thing 'binary-disjunction))

(defmethod print-object ((bin-dis binary-disjunction) stream)
  (format stream "|"))

(defgeneric make-binary-disjunction (lhs rhs))

(defclass multiple-arity-disjunction (multiple-arity-connective-formula)
  nil)

(defgeneric disjuncts (formula))

(defmethod disjuncts ((x t))
  (list x))

(defmethod disjuncts ((x binary-disjunction))
  (append (disjuncts (lhs x))
	  (disjuncts (rhs x))))

(defmethod disjuncts ((x multiple-arity-disjunction))
  (disjuncts (items x)))

(defmethod disjuncts ((x null))
  nil)

(defmethod disjuncts ((l list))
  (reduce #'append (mapcar #'disjuncts l)))

(defmethod disjuncts ((x multiple-arity-disjunction))
  (reduce #'append (mapcar #'disjuncts (items x))))

(defmethod connective-unit ((mad multiple-arity-disjunction))
  (declare (ignore mad))
  top)

(defun multiple-arity-disjunction? (thing)
  (eql (class-of thing) 'multiple-arity-disjunction))

(defmethod print-object ((mad multiple-arity-disjunction) stream)
  (format stream "|"))

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
	      (make-instance 'binary-disjunction
			     :lhs (first disjuncts)
			     :rhs (second disjuncts)))
	  (first disjuncts))
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

(defmethod print-object ((con binary-conjunction) stream)
  (format stream "&"))

(defgeneric conjuncts (formula))

(defmethod conjuncts ((x t))
  (list x))

(defmethod conjuncts ((x binary-conjunction))
  (append (conjuncts (lhs x))
	  (conjuncts (rhs x))))

(defmethod conjuncts ((x null))
  nil)

(defmethod conjuncts ((l list))
  (reduce #'append (mapcar #'conjuncts l)))

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defmethod conjuncts ((x multiple-arity-conjunction))
  (conjuncts (items x)))

(defmethod connective-unit ((mac multiple-arity-conjunction))
  (declare (ignore mac))
  contradiction)

(defun multiple-arity-conjunction? (thing)
  (eql (class-of thing) 'multiple-arity-conjunction))

(defmethod print-object ((mac multiple-arity-conjunction) stream)
  (format stream "&"))

(defun make-binary-conjunction (lhs rhs)
  (make-instance 'binary-conjunction
		 :lhs lhs
		 :rhs rhs))

(defun make-multiple-arity-conjunction (&rest conjuncts)
  (if conjuncts
      (if (cdr conjuncts)
	  (if (cddr conjuncts)
	      (make-instance 'multiple-arity-conjunction
			     :items conjuncts)
	      (make-instance 'binary-conjunction
			     :lhs (first conjuncts)
			     :rhs (second conjuncts)))
	  (first conjuncts))
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

(defmethod print-object ((uni-gen universal-generalization) stream)
  (format stream "(! [~{~a~^,~}] : ~a)" (bindings uni-gen) (matrix uni-gen)))

(defun existential-generalization? (thing)
  (eql (class-of thing) 'existential-generalization))

(defmethod print-object ((exi-gen existential-generalization) stream)
  (format stream "(? [~{~a~^,~}] : ~a)" (bindings exi-gen) (matrix exi-gen)))

(defun make-universal (bindings formula)
  (make-instance 'universal-generalization
		 :bindings bindings
		 :matrix formula))

(defun make-existential (bindings matrix)
  (make-instance 'existential-generalization
		 :bindings bindings
		 :matrix matrix))

(defgeneric form->formula (thing)
  (:documentation "Given THING, try to make sense of it as a formula."))

(defgeneric op-and-args->formula (operator arguments)
  (:documentation "Try to understand a symbol OPERATOR and a list ARGUMENTS as a formula."))

(defgeneric op-and-args->term (operator arguments)
  (:documentation "Try to understand a symbol OPERATOR and a list ARGUMENTS as a term."))

;; By default, make atoms
(defmethod op-and-args->formula ((op symbol) arguments)
  (apply #'make-atomic-formula op arguments))

(defmethod op-and-args->formula ((op (eql *exists-symbol*)) arguments)
  (destructuring-bind (bindings matrix)
      arguments
    (make-existential (mapcar #'form->term bindings)
		      (form->formula matrix))))

(defmethod op-and-args->formula ((op (eql *all-symbol*)) arguments)
  (destructuring-bind (bindings matrix)
      arguments
    (make-universal (mapcar #'form->term bindings)
		    (form->formula matrix))))

(defmethod op-and-args->formula ((op (eql *or-symbol*)) arguments)
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

(defmethod op-and-args->formula ((op (eql *and-symbol*)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((first-conjunct (form->formula (first arguments)))
		    (second-conjunct (form->formula (second arguments))))
		(make-binary-conjunction first-conjunct second-conjunct))
	      (let ((conjuncts (mapcar #'form->formula arguments)))
		(apply #'make-multiple-arity-conjunction conjuncts))))))

(defmethod op-and-args->formula ((op (eql *negation-symbol*)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (let ((negated (form->formula (car arguments))))
	    (negate negated))
	  (error 'parse-form-unary-operator-multiple-arguments-error
		 :operator op))))

(defmethod op-and-args->formula ((op (eql *implication-symbol*)) arguments)
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
		     :arguments arguments)))))

(defmethod op-and-args->formula ((op (eql *equivalence-symbol*)) arguments)
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

(defmethod op-and-args->formula ((op (eql *nonequivalence-symbol*)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((lhs (form->formula (car arguments)))
		    (rhs (form->formula (cadr arguments))))
		(make-nonequivalence lhs rhs))
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

(defclass tptp-source ()
  nil)

(defclass inference-record (tptp-source)
  ((rule
    :accessor rule
    :initarg :rule
    :initform (error "An inference record needs a rule of inference"))
   (useful-info
    :type general-list
    :accessor useful-info
    :initarg :useful-info
    :initform (make-instance 'general-list))
   (parents
    :type general-list
    :accessor parents
    :initarg :parents
    :initform (make-instance 'general-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flatten
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric flatten-tptp (tptp-thing)
  (:documentation "All the atoms that can be reached from TPTP-THING."))

(defmethod flatten-tptp ((unhandled-tptp-thing t))
  (let ((class (class-of unhandled-tptp-thing)))
    (error "Don't know how to flatten TPTP things of class ~a." class)))

(defmethod flatten-tptp ((tptp-thing string))
  (list tptp-thing))

(defmethod flatten-tptp ((ir inference-record))
  (append (flatten-tptp (rule ir))
	  (flatten-tptp (useful-info ir))
	  (flatten-tptp (parents ir))))

(defmethod flatten-tptp ((tptp-thing cons))
  (append (flatten-tptp (car tptp-thing))
	  (flatten-tptp (cdr tptp-thing))))

(defmethod flatten-tptp ((tptp-thing null))
  nil)

(defmethod flatten-tptp ((tptp-thing integer))
  (list tptp-thing))

(defmethod flatten-tptp ((tptp-atom atomic-expression))
  (with-slots (head arguments)
      tptp-atom
    (if (and (string= (stringify head) "file")
	     (length= 2 arguments))
	(list tptp-atom) ;; this looks like an external source
	(apply #'append
	       (list (head tptp-atom))
	       (mapcar #'flatten-tptp (arguments tptp-atom))))))

(defmethod flatten-tptp ((tptp-atom atomic-formula))
  (apply #'append
	 (list (predicate tptp-atom))
	 (mapcar #'flatten-tptp (arguments tptp-atom))))

(defmethod flatten-tptp ((var variable-term))
  (list var))

(defmethod flatten-tptp ((l general-list))
  (flatten-tptp (terms l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Kowalski form for clauses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric kowalski (formula))

(defmethod kowalski ((x generalization))
  (make-instance (class-of x)
		 :bindings (bindings x)
		 :matrix (kowalski (matrix x))))

(defmethod kowalski ((x binary-connective-formula))
  (make-instance (class-of x)
		 :lhs (kowalski (lhs x))
		 :rhs (kowalski (rhs x))))

(defmethod kowalski ((x implication))
  (let ((a (antecedent x))
        (c (consequent x)))
    (cond ((constant-true-p a)
           (kowalski c))
          ((constant-false-p a)
           contradiction)
          ((constant-true-p c)
           top)
          ((constant-false-p c)
           (kowalski (negate a)))
          (t
           (call-next-method)))))

(defmethod kowalski ((x atomic-formula))
  x)

(defmethod kowalski ((x negation))
  (make-instance 'negation
		 :argument (kowalski (argument x))))

(defgeneric negative-formula-p (formula))

(defmethod negative-formula-p ((formula t))
  nil)

(defmethod negative-formula-p ((neg negation))
  t)

(defmethod negative-formula-p ((eq disequation))
  t)

(defgeneric positivize (formula))

(defmethod positivize ((formula t))
  formula)

(defmethod positivize ((formula negation))
  (argument formula))

(defmethod positivize ((eq disequation))
  (make-instance 'equation
		 :lhs (lhs eq)
		 :rhs (rhs eq)))

(defmethod kowalski ((x multiple-arity-disjunction))
  (let ((disjuncts (disjuncts x)))
    (setf disjuncts (remove-if #'(lambda (x) (and (typep x 'atomic-formula)
						  (length= 0 (arguments x))
						  (string= (stringify (predicate x))
							   "false")))
			       disjuncts))
    (let ((negated-disjuncts (remove-if-not #'negative-formula-p disjuncts))
	  (non-negated-disjuncts (remove-if #'negative-formula-p disjuncts)))
      (if negated-disjuncts
	  (if non-negated-disjuncts
	      (kowalski (make-implication (apply #'make-multiple-arity-conjunction negated-disjuncts)
					  (apply #'make-multiple-arity-disjunction non-negated-disjuncts)))
	      (kowalski (make-implication (apply #'make-multiple-arity-conjunction negated-disjuncts)
					  *nullary-false*)))
	  (apply #'make-multiple-arity-disjunction
		 (mapcar #'kowalski non-negated-disjuncts))))))

(defmethod kowalski ((x multiple-arity-conjunction))
  (make-instance 'multiple-arity-conjunction
		 :items (mapcar #'kowalski (items x))))

(defmethod kowalski ((x binary-disjunction))
  (let ((disjuncts (disjuncts x)))
    (setf disjuncts (remove-if #'(lambda (x) (and (typep x 'atomic-formula)
						  (length= 0 (arguments x))
						  (string= (stringify (predicate x))
							   "false")))
			       disjuncts))
    (let ((negated-disjuncts (remove-if-not #'negative-formula-p disjuncts))
	  (non-negated-disjuncts (remove-if #'negative-formula-p disjuncts)))
      (if negated-disjuncts
	  (if non-negated-disjuncts
	      (kowalski (make-implication (apply #'make-multiple-arity-conjunction (mapcar #'positivize negated-disjuncts))
					  (apply #'make-multiple-arity-disjunction non-negated-disjuncts)))
	      (kowalski (make-implication (apply #'make-multiple-arity-conjunction (mapcar #'positivize negated-disjuncts))
					  *nullary-false*)))
	  (apply #'make-multiple-arity-disjunction
		 (mapcar #'kowalski non-negated-disjuncts))))))

(defun same-variable-name (variable-1 variable-2)
  (string= (stringify (head variable-1))
	   (stringify (head variable-2))))

(defgeneric free-variables (expression))

(defmethod free-variables :around ((expression t))
  (let ((free (call-next-method)))
    (remove-duplicates free :test #'same-variable-name :from-end t)))

(defmethod free-variables ((gen generalization))
  (with-slots (bindings matrix)
      gen
    (set-difference (free-variables matrix) bindings
		    :test #'same-variable-name)))

(defmethod free-variables ((x atomic-formula))
  (remove-if-not #'variable-term-p (flatten-tptp x)))

(defmethod free-variables ((x binary-connective-formula))
  (append (free-variables (lhs x))
	  (free-variables (rhs x))))

(defmethod free-variables ((x multiple-arity-connective-formula))
  (reduce #'append (mapcar #'free-variables (items x))))

(defmethod free-variables ((x negation))
  (free-variables (argument x)))

(defgeneric universally-close (x))

(defmethod universally-close ((formula formula))
  (let ((free (free-variables formula)))
    (if (null free)
	formula
	(make-instance 'universal-generalization
		       :bindings free
		       :matrix formula))))

(defmethod print-object ((ir inference-record) stream)
  (format stream "inference(~a,~a,~a)" (rule ir) (useful-info ir) (parents ir)))

(defgeneric flatten-conjunctions/disjunctions (tptp-thing))

(defmethod flatten-conjunctions/disjunctions ((x atomic-formula))
  x)

(defmethod flatten-conjunctions/disjunctions ((x multiple-arity-conjunction))
  (apply #'make-multiple-arity-conjunction
	 (mapcar #'flatten-conjunctions/disjunctions (conjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x multiple-arity-disjunction))
  (apply #'make-multiple-arity-disjunction
	 (mapcar #'flatten-conjunctions/disjunctions (disjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x binary-disjunction))
  (apply #'make-multiple-arity-disjunction
	 (mapcar #'flatten-conjunctions/disjunctions (disjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x binary-conjunction))
  (apply #'make-multiple-arity-conjunction
	 (mapcar #'flatten-conjunctions/disjunctions (conjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x negation))
  (make-instance 'negation
		 :argument (flatten-conjunctions/disjunctions (argument x))))

(defmethod flatten-conjunctions/disjunctions ((x binary-connective-formula))
  (make-instance (class-of x)
		 :lhs (flatten-conjunctions/disjunctions (lhs x))
		 :rhs (flatten-conjunctions/disjunctions (rhs x))))

(defmethod flatten-conjunctions/disjunctions ((gen generalization))
  (make-instance (class-of gen)
		 :bindings (bindings gen)
		 :matrix (flatten-conjunctions/disjunctions (matrix gen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Syntactic) equality of terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equal-terms-p (term-1 term-2)
  (:documentation "Are terms TERM-1 and TERM-2 equal?"))

(defmethod equal-terms-p ((term-1 t) (term-2 t))
  nil)

(defmethod equal-terms-p ((term-1 atomic-expression) (term-2 atomic-expression))
  (let ((head-1 (head term-1))
	(head-2 (head term-2))
	(arguments-1 (arguments term-1))
	(arguments-2 (arguments term-2)))
    (when (string= (stringify head-1) (stringify head-2))
      (when (length= arguments-1 arguments-2)
	(loop
	   :for argument-1 :in arguments-1
	   :for argument-2 :in arguments-2
	   :unless (equal-terms-p argument-1 argument-2) :do (return nil)
	   :finally (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hunting for terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric terms-with-functor (functor-name tptp)
  (:documentation "The (function) terms inside TPTP whose functor is FUNCTOR-NAME."))

(defmethod terms-with-functor (functor-name (x null))
  nil)

(defmethod terms-with-functor (functor-name (l list))
  (let ((terms (mapcar #'(lambda (x) (terms-with-functor functor-name x)) l)))
    (remove-duplicates (reduce #'append terms) :test #'equal-terms-p)))

(defmethod terms-with-functor (functor-name (atom atomic-formula))
  (terms-with-functor functor-name (arguments atom)))

(defmethod terms-with-functor (functor-name (neg negation))
  (terms-with-functor functor-name (argument neg)))

(defmethod terms-with-functor (functor-name (x binary-connective-formula))
  (terms-with-functor functor-name (list (lhs x) (rhs x))))

(defmethod terms-with-functor (functor-name (x multiple-arity-connective-formula))
  (terms-with-functor functor-name (items x)))

(defmethod terms-with-functor (functor-name (gen generalization))
  (terms-with-functor functor-name (matrix gen)))

(defmethod terms-with-functor (functor-name (var variable-term))
  nil)

(defmethod terms-with-functor (functor-name (term atomic-expression))
  (if (string= (stringify functor-name)
	       (stringify (head term)))
      (cons term (terms-with-functor functor-name (arguments term)))
      (terms-with-functor functor-name (arguments term))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting formula that contain $true and $false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric eliminate-truth-values (thing)
  (:documentation "Rewrite THING to get rid (as far as possible) of
  any constant true or constant false expressions."))

(defgeneric constant-true-p (thing))

(defmethod constant-true-p ((x atomic-formula))
  (unless (arguments x)
    (string= (stringify (predicate x)) "true")))

(defmethod constant-true-p ((x t))
  nil)

(defgeneric constant-false-p (thing))

(defmethod constant-false-p ((x atomic-formula))
  (unless (arguments x)
    (string= (stringify (predicate x)) "false")))

(defmethod constant-false-p ((x t))
  nil)

(defmethod eliminate-truth-values ((x atomic-formula))
  x)

(defmethod eliminate-truth-values ((x equation))
  x)

(defmethod eliminate-truth-values ((x disequation))
  x)

(defmethod eliminate-truth-values ((x binary-disjunction))
  (with-slots (lhs rhs)
      x
    (cond ((constant-true-p lhs)
	   *nullary-true*)
	  ((constant-false-p lhs)
	   (eliminate-truth-values rhs))
	  ((constant-true-p rhs)
	   *nullary-true*)
	  ((constant-false-p rhs)
	   (eliminate-truth-values lhs))
	  (t
	   (make-instance 'binary-disjunction
			  :lhs (eliminate-truth-values lhs)
			  :rhs (eliminate-truth-values rhs))))))

(defmethod eliminate-truth-values ((x multiple-arity-disjunction))
  (let ((items (items x)))
    (if (find-if #'constant-true-p items)
	*nullary-true*
	(let ((no-false (remove-if #'constant-false-p items)))
	  (apply #'make-multiple-arity-disjunction
		 (mapcar #'eliminate-truth-values no-false))))))

(defmethod eliminate-truth-values ((x binary-conjunction))
  (with-slots (lhs rhs)
      x
    (cond ((constant-true-p lhs)
	   (eliminate-truth-values rhs))
	  ((constant-false-p lhs)
	   *nullary-false*)
	  ((constant-true-p rhs)
	   (eliminate-truth-values lhs))
	  ((constant-false-p rhs)
	   *nullary-false*)
	  (t
	   (make-instance 'binary-conjunction
			  :lhs (eliminate-truth-values lhs)
			  :rhs (eliminate-truth-values rhs))))))

(defmethod eliminate-truth-values ((x multiple-arity-conjunction))
  (let ((items (items x)))
    (if (find-if #'constant-false-p items)
	*nullary-false*
	(let ((no-true (remove-if #'constant-true-p items)))
	  (apply #'make-multiple-arity-conjunction
		 (mapcar #'eliminate-truth-values no-true))))))

(defmethod eliminate-truth-values ((neg negation))
  (let ((argument (argument neg)))
    (cond ((constant-true-p argument)
	   *nullary-false*)
	  ((constant-false-p argument)
	   *nullary-true*)
	  (t
	   (make-instance 'negation
			  :argument (eliminate-truth-values argument))))))

(defmethod eliminate-truth-values ((x implication))
  (with-slots ((antecedent lhs) (consequent rhs))
      x
    (cond ((constant-true-p antecedent)
	   (eliminate-truth-values consequent))
	  ((constant-true-p consequent)
	   *nullary-true*)
	  ((constant-false-p antecedent)
	   *nullary-true*)
	  ((constant-false-p consequent)
	   (eliminate-truth-values (make-instance 'negation
						  :argument (eliminate-truth-values antecedent))))
	  (t
	   (make-instance 'implication
			  :lhs (eliminate-truth-values antecedent)
			  :rhs (eliminate-truth-values consequent))))))

(defmethod eliminate-truth-values ((x equivalence))
  (with-slots (lhs rhs)
      x
    (cond ((constant-true-p lhs)
	   (eliminate-truth-values rhs))
	  ((constant-true-p rhs)
	   (eliminate-truth-values lhs))
	  ((constant-false-p lhs)
	   (eliminate-truth-values (make-instance 'negation
						  :argument rhs)))
	  ((constant-false-p rhs)
	   (eliminate-truth-values (make-instance 'negation
						  :argument lhs)))
	  (t
	   (make-instance 'equivalence
			  :lhs (eliminate-truth-values lhs)
			  :rhs (eliminate-truth-values rhs))))))

(defmethod eliminate-truth-values ((gen generalization))
  (make-instance (class-of gen)
		 :bindings (bindings gen)
		 :matrix (eliminate-truth-values (matrix gen))))

(defmethod eliminate-truth-values ((x null))
  nil)

(defmethod eliminate-truth-values ((l list))
  (mapcar #'eliminate-truth-values l))

(defgeneric contains-predicate? (expression predicate)
  (:documentation "Does EXPRESSION contain a subexpression with the predicate symbol PREDICATE?"))

(defmethod contains-predicate? ((atom atomic-expression) predicate)
  nil)

(defmethod contains-predicate? ((term function-term) predicate)
  nil)

(defmethod contains-predicate? ((term variable-term) predicate)
  nil)

(defmethod contains-predicate? ((atom atomic-formula) predicate)
  (string= (stringify (predicate atom)) (stringify predicate)))

(defmethod contains-predicate? ((neg negation) predicate)
  (contains-predicate? (argument neg) predicate))

(defmethod contains-predicate? ((multi multiple-arity-connective-formula) predicate)
  (some #'(lambda (x) (contains-predicate? x predicate))
	(items multi)))

(defmethod contains-predicate? ((x binary-connective-formula) predicate)
  (or (contains-predicate? (lhs x) predicate)
      (contains-predicate? (rhs x) predicate)))

(defmethod contains-predicate? ((gen generalization) predicate)
  (contains-predicate? (matrix gen) predicate))

;;; formulas.lisp ends here
