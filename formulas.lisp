;;; formulas.lisp A representation for first-order formulas

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Signatures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass signature ()
  ((constants :initarg :constants
	      :initform nil
	      :type list
	      :accessor signature-constants)
   (predicates :initarg :predicates
	       :initform nil
	       :type list
	       :accessor signature-predicates)
   (functions :initarg :functions
	      :initform nil
	      :type list
	      :accessor signature-functions)))

(define-condition unacceptable-identifier-name-error (error)
  ((text :initarg :text
	 :reader unacceptable-identifier-name-error-text))
  (:report (lambda (condition stream)
	     (let ((text (unacceptable-identifier-name-error-text condition)))
	       (format stream "The given string,~%~%  ~A,~%~%is an unacceptable as the name of an identifier." text)))))

(defun try-another-identifier (c)
  (declare (ignore c))
  (let ((restart (find-restart 'try-another-identifier)))
    (when restart
      (invoke-restart 'try-another-identifier))))

(define-condition symbol-already-present-error (error)
  ((symbol :initarg :symbol
	   :reader symbol-already-present-error-symbol)
   (signature :initarg :signature 
	      :reader symbol-already-present-error-signature))
  (:report (lambda (condition stream)
	     (let ((symbol (symbol-already-present-error-symbol condition))
		   (sig (symbol-already-present-error-signature condition)))
	       (if sig
		   (if symbol
		       (format stream 
			       "The given symbol,~%~%  ~A,~%~%already belongs to the signature~%~%  ~A~%"
			       symbol sig)
		       (format stream
			       "Weird: although a signature,~%~%  ~A~%~%was supplied, no symbol was given (or possibly what was given was the symbol NIL).  It would seem that this error condition was not properly initialized by the calling code.~%"
			       sig))
		   (if symbol
		       (format stream
			       "Weird: unable to determine whether the given symbol,~%~%  ~A,~%~%aready belongs to the signature because no signature was supplied.  It woud seem that this error condition was not properly initialized by the calling code.~%"
			       symbol)
		       (format stream
			       "Weird: neither a signature nor a text were supplied. It would seem that this error condition was not properly initialized by the calling code.~%")))))))

(defun copy-signature (signature)
  (make-instance 'signature
		 :constants (copy-list (signature-constants signature))
		 :predicates (copy-list (signature-predicates signature))
		 :functions (copy-list (signature-functions signature))))

(defmethod print-object ((sig signature) stream)
  (print-unreadable-object (sig stream :type t)
    (with-slots (constants predicates functions) sig
      (format stream "constants: ~A, functions: ~A, predicates: ~A" 
	      (if constants
		  (comma-separated-list constants)
		  "(none)")
	      (or functions "(none)")
	      (or predicates "(none)")))))

(defgeneric belongs-to-signature? (signature thing))
(defgeneric add-constant (signature new-name))
(defgeneric delete-constant (signature name))
(defgeneric add-function (signature new-name new-arity))
(defgeneric delete-function (signature name))
(defgeneric add-predicate (signature new-name new-arity))
(defgeneric delete-predicate (signature name))

(defun equal-signatures? (signature-1 signature-2)
  (and (equal-sets? (signature-constants signature-1)
		    (signature-constants signature-2)
		    :test #'eq)
       (equal-sets? (signature-predicates signature-1)
		    (signature-predicates signature-2)
		    :test #'equal) 
       (equal-sets? (signature-functions signature-1)
		    (signature-functions signature-2)
		    :test #'equal)))

(defun make-signature-with-equality (&key constants predicates functions)
  (make-instance 'signature 
		 :constants constants
		 :predicates (cons '= predicates)
		 :functions functions))

(defun functions-of-arity (signature arity)
  (let (result)
    (dolist (symbol-and-arity (signature-functions signature) result)
      (let ((sym (car symbol-and-arity))
	    (num-args (cdr symbol-and-arity)))
	(when (= num-args arity)
	  (push sym result))))))

(defun function-of-arity (signature function-symbol arity)
  (some #'(lambda (symbol-and-arity)
	    (let ((symbol (car symbol-and-arity))
		  (num-args (cdr symbol-and-arity)))
	      (when (eq symbol function-symbol)
		(= arity num-args))))
	(signature-functions signature)))

(defun predicates-of-arity (signature arity)
  (let (result)
    (dolist (symbol-and-arity (signature-predicates signature) result)
      (let ((sym (car symbol-and-arity))
	    (num-args (cdr symbol-and-arity)))
	(when (= num-args arity)
	  (push sym result))))))

(defun predicate-of-arity (signature relation-symbol arity)
  (some #'(lambda (symbol-and-arity)
	    (let ((symbol (car symbol-and-arity))
		  (num-args (cdr symbol-and-arity)))
	      (when (string= (symbol-name symbol)
			     (symbol-name relation-symbol))
		(= arity num-args))))
	(signature-predicates signature)))

(defgeneric constant? (signature sym))
(defgeneric function? (signature sym))
(defgeneric predicate? (signature sym))

(defmethod constant? ((s signature) sym)
  (member sym (signature-constants s)))

(defmethod predicate? ((s signature) pred-sym)
  (some #'(lambda (sym) (eq sym pred-sym))
	(mapcar #'car (signature-predicates s))))

(defmethod function? ((s signature) func-sym)
  (some #'(lambda (sym) (eq sym func-sym))
	(mapcar #'car (signature-functions s))))

(defun valid-identifier-name? (str)
  (and str
       (not (empty-string? str))
       (not (contains-whitespace? str))))

(defmethod belongs-to-signature? ((sig signature) (sym symbol))
  (or (constant? sig sym) 
      (predicate? sig sym)
      (function? sig sym)))

(defmethod belongs-to-signature? ((sig signature) (str string))
  (belongs-to-signature? sig (symbolify str)))

(defmethod add-constant ((sig signature) (constant-sym symbol))
  (cond ((belongs-to-signature? sig constant-sym)
	 (error 'symbol-already-present-error
		:symbol constant-sym
		:signature sig))
	(t (push constant-sym (signature-constants sig))
	   sig)))

(defmethod add-constant ((sig signature) (constant-str string))
  (if (valid-identifier-name? constant-str)
      (add-constant sig (symbolify constant-str))
      (error 'unacceptable-identifier-name-error
	     :text constant-str)))

(defmethod delete-constant ((sig signature) (constant-sym symbol))
  (setf (signature-constants sig)
	(remove constant-sym (signature-constants sig))))

(defmethod delete-constant ((sig signature) (constant-str string))
  (delete-constant sig (symbolify constant-str)))

(defmethod add-predicate ((sig signature) (pred-sym symbol) (arity integer))
  (cond ((belongs-to-signature? sig pred-sym)
	 (error 'symbol-already-present-error
		:symbol pred-sym
		:signature sig))
	(t (setf (signature-predicates sig)
		 (acons pred-sym arity (signature-predicates sig)))
	   sig)))

(defmethod add-predicate ((sig signature) (pred-str string) (arity integer))
  (if (valid-identifier-name? pred-str)
      (add-predicate sig (symbolify pred-str) arity)
      (error 'unacceptable-identifier-name-error
	     :text pred-str)))

(defmethod delete-predicate ((sig signature) (pred-sym symbol))
  (setf (signature-predicates sig)
	(remove (find pred-sym (signature-predicates sig) :key #'first)
		(signature-predicates sig))))

(defmethod delete-predicate ((sig signature) (pred-str string))
  (delete-predicate sig (symbolify pred-str)))

(defmethod add-function ((sig signature) (func-sym symbol) (arity integer))
  (cond ((belongs-to-signature? sig func-sym)
	 (error 'symbol-already-present-error
		:symbol func-sym
		:signature sig))
	(t
	 (setf (signature-functions sig)
	       (acons func-sym arity (signature-functions sig)))
	 sig)))

(defmethod add-function ((sig signature) (func-str string) (arity integer))
  (if (valid-identifier-name? func-str)
      (add-function sig (symbolify func-str) arity)
      (error 'unaccepptable-identifier-name-error
	     :symbol (symbolify func-str))))

(defmethod delete-function ((sig signature) func-sym)
  (setf (signature-functions sig)
	(remove (find func-sym (signature-functions sig) :key #'first)
		(signature-functions sig))))

(defmethod delete-function ((sig signature) (func-str string))
  (delete-function sig (symbolify func-str)))

(defun read-signature (prompt)
  (let (constants predicates functions)
    (tagbody (go start)
     start
       (go constants)
     constants
       (yes-or-no-go "Do you want to input any constant symbols?"
		     prompt
		     read-constant
		     functions)
     read-constant
       (with-simple-prompt (prompt)
	 "Input a symbol for the new constant:"
	 (push (read-symbol) constants))
       (yes-or-no-go "Enter more constants?"
		     prompt
		     read-constant
		     functions)
     functions
       (yes-or-no-go "Do you want to input any function symbols?"
		     prompt
		     read-function
		     predicates)
     read-function
       (let (func-sym arity)
	 (with-simple-prompt (prompt)
	     "Input a symbol for the new function:"
	 (setf func-sym (read-symbol)))
	 (msg "What arity does ~A have?" func-sym)
	 (format t "~A" prompt)
	 (setf arity (read-natural-number))
	 (push (cons func-sym arity) functions)
	 (yes-or-no-go "Enter more function symbols?"
		       prompt
		       read-function
		       predicates))
     predicates
       (yes-or-no-go "Do you want to input any predicates?"
		     prompt
		     read-predicate
		     check)
     read-predicate
       (let (pred-sym arity)
	 (with-simple-prompt (prompt)
	     "Input a symbol for the new predicate:"
	   (setf pred-sym (read-symbol)))
	 (msg "What arity does ~A have?" pred-sym)
	 (format t "~A" prompt)
	 (setf arity (read-natural-number))
	 (push (cons pred-sym arity) predicates)
	 (yes-or-no-go "Enter more predicates?"
		       prompt
		       read-predicate
		       check))
     check
       (when predicates
	 (msg "The signature looks like this:")
	 (msg "Constants: ~A" (or constants "(none)"))
	 (msg "Predicates: ~A" predicates)
	 (msg "Functions: ~A" (or functions "(none)"))
	 (yes-or-no-go "Do you want to add anything else to the signature?"
		       prompt
		       start
		       end))
       (msg "No predicates have been entered; you won't be able to say anything!")
       (msg "Returning to the first prompt...")
       (go start)
     end)
    (make-instance 'signature
		   :predicates predicates
		   :functions functions
		   :constants constants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concrete signatures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter pqrs-propositional-signature
  (make-instance 'signature
		 :predicates '((p . 0)
			       (q . 0)
			       (r . 0)
			       (s . 0))))

(defparameter unary-pqrs-signature-with-equality
  (make-signature-with-equality :predicates '((p . 1)
					      (q . 1)
					      (r . 1)
					      (s . 1))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equal-variables? (var-1 var-2)
  (eql var-1 var-2))

(defun variable? (term)
  (and (symbolp term)
       (eq (aref (symbol-name term) 0) #\?)))

(defun make-variable (symbol-or-string)
  (let ((name (if (symbolp symbol-or-string)
			   (symbol-name symbol-or-string)
			   symbol-or-string)))
    (cond ((string= name "")
	   (error "One cannot make a variable with an empty name"))
	  ((char= (char name 0) #\?)
	   (error "Variables already begin with a question mark; unclear how to proceed"))
	  (t (make-symbol (concatenate 'string "?" name))))))

(defun term? (x signature)
  (when x
    (or (variable? x)
	(member x (signature-constants signature))
	(and (listp x)
	     (let* ((function-symbol (car x))
		    (args (cdr x))
		    (num-args (length args)))
	       (and (function-of-arity signature function-symbol num-args))
		    (every #'(lambda (arg) (term? arg signature))
			   args))))))

(defun function-symbol (complex-term)
  (car complex-term))

(defun term-arguments (complex-term)
  (cdr complex-term))

(defun make-complex-term (function &rest args)
  (cons function args))

(defun equal-terms? (term-1 term-2 signature)
  (declare (ignore signature))
  (equalp term-1 term-2))

(defun bare-variable? (variable)
  (symbolp variable))

(defun typed-variable? (variable)
  (not (bare-variable? variable)))

(defun variable-type (typed-variable)
  (second typed-variable))

(defun variable-name (typed-variable)
  (first typed-variable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant contradiction '⊥)
(defconstant top '⊤)

(defun top? (formula)
  (eq (car formula) top))

(defun bottom? (formula)
  (eq (car formula) contradiction))

(defun negation? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
	(and (symbolp s)
	     (string= s "NOT"))))))

(defun unnegate (negation)
  (second negation))

(defun negate (formula)
  (list 'not formula))

(defun implication? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
      (and (symbolp s)
	   (string= (symbol-name s) "IMPLIES"))))))

(defun antecedent (implication)
  (cadr implication))

(defun consequent (implication)
  (caddr implication))

(defun make-implication (antecedent consequent)
  (list 'implies antecedent consequent))

(defun equivalence? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
	(and (symbolp s)
	     (string= (symbol-name s) "IFF"))))))

(defun lhs (equivalence)
  (second equivalence))

(defun rhs (equivalence)
  (third equivalence))

(defun disjunction? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
	(and (symbolp s)
	     (string= (symbol-name s) "OR"))))))

(defun disjuncts (disjunction)
  (cdr disjunction))

(defun make-disjunction (&rest disjuncts)
  (cons 'or disjuncts))

(defun left-disjunct (disjunction)
  (cadr disjunction))

(defun right-disjunct (disjunction)
  (caddr disjunction))

(defun conjunction? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
	(and (symbolp s)
	     (string= (symbol-name s) "AND"))))))

(defun conjuncts (conjunction)
  (cdr conjunction))

(defun left-conjunct (conjunction)
  (cadr conjunction))

(defun right-conjunct (conjunction)
  (caddr conjunction))

(defun make-conjunction (&rest conjuncts)
  (cons 'and conjuncts))

(defun universal? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
	(and (symbolp s)
	     (string= (symbol-name s) "ALL"))))))

(defun make-universal (var formula)
  (list 'all var formula))

(defun existential? (formula)
  (when formula
    (when (listp formula)
      (let ((s (car formula)))
	(and (symbolp s)
	     (string= (symbol-name s) "EXISTS"))))))

(defun make-existential (var formula)
  (list 'exists var formula))

(defun matrix (quantified-formula)
  (caddr quantified-formula))

(defun bound-variable (quantified-formula)
  (cadr quantified-formula))

(defun atomic-formula? (x signature)
  (when x
    (if (symbolp x)
	(predicate-of-arity signature x 0)
	(when (listp x)
	  (let* ((pred (car x))
		 (args (cdr x))
		 (num-args (length args)))
	    (and (predicate-of-arity signature pred num-args)
		 (every #'(lambda (arg) (term? signature arg))
			args)))))))

(defun formula? (x signature)
  (when x
    (or (atomic-formula? x signature)
	(and (conjunction? x) 
	     (formula? (left-conjunct x) signature)
	     (formula? (right-conjunct x) signature))
	(and (disjunction? x)
	     (formula? (left-disjunct x) signature)
	     (formula? (right-disjunct x) signature))
	(and (implication? x)
	     (formula? (antecedent x) signature)
	     (formula? (consequent x) signature))
	(and (negation? x)
	     (formula? (unnegate x) signature))
	(and (universal? x)
	     (variable? (bound-variable x))
	     (formula? (matrix x) signature))
	(and (existential? x)
	     (variable? (bound-variable x))
	     (formula? (matrix x) signature)))))

(defun composite-formula? (x signature)
  (and (formula? x signature)
       (not (atomic-formula? x signature))))

(defun predicate (atomic-formula)
  (car atomic-formula))

(defun arguments (atomic-formula)
  (cdr atomic-formula))

(defun make-atomic-formula (predicate &rest arguments)
  (cons predicate arguments))

(defun equal-formulas? (form-1 form-2 signature)
  "Determine whether formulas FORM-1 and FORM-2 are equal."
  (declare (ignore signature)) ;; A bit unsatisfactory, but it'll do for now
  (equalp form-1 form-2))

(defun equation? (formula)
  (eq (car formula) '=))

(defun make-equation (lhs rhs)
  (list '= lhs rhs))

(defun unary-statement-argument (unary-statement)
  (cadr unary-statement))

(defun binary-statement-first-arg (binary-statement)
  (cadr binary-statement))

(defun binary-statement-second-arg (binary-statement)
  (caddr binary-statement))

(defun make-atomic-statement (predicate &rest args)
  (cons predicate args))

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
		(let ((left (left-disjunct f))
		      (right (right-disjunct f)))
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

(defun make-sequent (signature lhs rhs)
  (if (formula? lhs signature)
      (if (formula? rhs signature)
	  (make-seq :lhs (list lhs)
		    :rhs (list rhs))
	  (make-seq :lhs (list lhs)
		    :rhs rhs))
      (if (formula? rhs signature)
	  (make-seq :lhs lhs
		    :rhs (list rhs))
	  (make-seq :lhs lhs
		    :rhs rhs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sundry formula-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contains-formula? (lst formula)
  (member formula lst :test #'equal-formulas?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading terms and formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-term ()
  (let (response)
    (until (symbolp response)
      (setq response (read t nil nil)))
    response))

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