
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

(defgeneric equivalence->conjunction (tptp-thing)
  (:documentation "Replace equivalences by conjunctions of implications."))

(defmethod equivalence->conjunction ((x atomic-formula))
  x)

(defmethod equivalence->conjunction ((x multiple-arity-conjunction))
  (apply #'make-multiple-arity-conjunction
	 (mapcar #'equivalence->conjunction (conjuncts x))))

(defmethod equivalence->conjunction ((x multiple-arity-disjunction))
  (apply #'make-multiple-arity-disjunction
	 (mapcar #'equivalence->conjunction (disjuncts x))))

(defmethod equivalence->conjunction ((x binary-disjunction))
  (make-binary-disjunction (equivalence->conjunction (lhs x))
                           (equivalence->conjunction (rhs x))))

(defmethod equivalence->conjunction ((x binary-conjunction))
  (make-binary-conjunction (equivalence->conjunction (lhs x))
                           (equivalence->conjunction (rhs x))))

(defmethod equivalence->conjunction ((x negation))
  (negate (equivalence->conjunction (argument x))))

(defmethod equivalence->conjunction ((x equivalence))
  (make-binary-conjunction
   (make-implication (equivalence->conjunction (lhs x))
                     (equivalence->conjunction (rhs x)))
   (make-implication (equivalence->conjunction (rhs x))
                     (equivalence->conjunction (lhs x)))))

(defmethod equivalence->conjunction ((gen generalization))
  (make-instance (class-of gen)
		 :bindings (bindings gen)
		 :matrix (equivalence->conjunction (matrix gen))))

;;; expressions.lisp ends here
