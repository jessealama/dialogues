;;; signature.lisp Propositional and first-order signatures

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Signatures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass signature ()
  nil)

(defclass propositional-signature (signature)
  nil)

(defclass finite-variable-propositional-signature (propositional-signature)
  ((predicates :initarg :predicates
	       :type list
	       :accessor signature-predicates
	       :initform nil)))

(define-condition non-symbol-treated-as-predicate-symbol-error (error)
  ((item :initarg :item
	 :reader item))
  (:report (lambda (condition stream)
	     (let ((item (item condition)))
	       (format stream "The item~%~%  ~A~%~%cannot be treated as a predicate symbol because it is not a lisp symbol." item)))))

(defun make-finite-variable-propositional-signature (&rest predicate-letters)
  (if (null predicate-letters)
      (progn
	(warn 'empty-signature-warning)
	(make-instance 'finite-variable-propositional-signature))
      (progn
	(loop
	   with final-preds = nil
	   with trimmed-preds = (remove-duplicates predicate-letters
						   :test #'eql)
	   for pred-letter in trimmed-preds
	   do
	     (if (symbolp pred-letter)
		 (push pred-letter final-preds)
		 (error 'non-symbol-treated-as-predicate-symbol-error
			:item pred-letter))
	   finally 
	     (return (make-instance 'finite-variable-propositional-signature
				    :predicates (reverse final-preds)))))))

(defclass infinite-variable-propositional-signature (propositional-signature)
  ((predicate-test :initarg :predicate-symbol-test
		   :type function
		   :accessor predicate-symbol-test
		   :initform (lambda (pred-sym)
			       (declare (ignore pred-sym))
			       nil))))

(defclass first-order-signature (signature)
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

(defclass signature-w/equality (signature)
  nil)

(defclass finite-variable-first-order-signature (first-order-signature)
  ((variables :initarg :variables
	      :initform nil
	      :type list
	      :accessor signature-variables)))

(defclass finite-variable-first-order-signature-w/equality
    (finite-variable-first-order-signature signature-w/equality)
  nil)

(defclass infinite-variable-first-order-signature (first-order-signature)
  ((variable-test :initarg :variable-test-pred
		  :type function
		  :initform (lambda (var)
			      (declare (ignore var))
			      nil)
		  :accessor variable-test-pred)))

(defclass infinite-variable-first-order-signature-w/equality
    (infinite-variable-first-order-signature signature-w/equality)
  nil)

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

(defgeneric copy-signature (signature))

(defmethod copy-signature ((sig finite-variable-propositional-signature))
  (make-instance 'finite-variable-propositional-signature
		 :predicates (copy-list (signature-predicates sig))))

(defmethod copy-signature ((sig finite-variable-first-order-signature))
  (make-instance 'finite-variable-first-order-signature
		 :constants (copy-list (signature-constants sig))
		 :predicates (copy-list (signature-predicates sig))
		 :functions (copy-list (signature-functions sig))
		 :variables (copy-list (signature-variables sig))))

(defmethod copy-signature ((sig infinite-variable-first-order-signature))
  (make-instance 'infinite-variable-first-order-signature
		 :constants (copy-list (signature-constants sig))
		 :predicates (copy-list (signature-predicates sig))
		 :functions (copy-list (signature-functions sig))))

(defmethod copy-signature ((sig finite-variable-first-order-signature))
  (make-instance 'finite-variable-first-order-signature
		 :constants (copy-list (signature-constants sig))
		 :predicates (copy-list (signature-predicates sig))
		 :functions (copy-list (signature-functions sig))
		 :variables (copy-list (signature-variables sig))))

(defmethod print-object ((sig finite-variable-propositional-signature) stream)
  (print-unreadable-object (sig stream :type t)
    (with-slots (predicates) sig
      (if (null predicates)
	  (format stream "predicates: (none)")
	  (format stream "predicates: ~A" predicates)))))

(defmethod print-object ((sig finite-variable-first-order-signature) stream)
  (print-unreadable-object (sig stream :type t)
    (with-slots (variables constants predicates functions) sig
      (format stream "variables: ~A, constants: ~A, functions: ~A, predicates: ~A"
	      (if variables
		  (comma-separated-list variables)
		  "(none)")
	      (if constants
		  (comma-separated-list constants)
		  "(none)")
	      (or functions "(none)")
	      (or predicates "(none)")))))

(defmethod print-object ((sig infinite-variable-first-order-signature) stream)
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

(defun verify-name-and-arity-item (item)
  (if (consp item)
      (if (null (cdr item))
	  (error 'inappropriate-name-and-arity-item-error
		 :item item)
	    (let ((arity (cdr item)))
	      (or (integerp arity)
		  (error 'inappropriate-name-and-arity-error
			 :item item))))
      (error 'inappropriate-name-and-arity-error
	     :item item)))

(define-condition duplicate-names-with-different-arities-error (error)
  ((item :initarg :item
	       :reader item)
   (first-value :initarg :first-value
		:reader first-value)
   (second-value :initarg :second-value
		:reader second-value)
   (item-list :initarg :list
	      :reader item-list))
  (:report (lambda (condition stream)
	     (let ((item (item condition))
		   (1st-value (first-value condition))
		   (2nd-value (second-value condition))
		   (items (item-list condition)))
	       (format stream 
		       "The item~%~%  ~A~%~%occurs twice in the list~%~%  ~A~%~%with different values: the first is~%~%  ~A,~%~%and the second is ~%~%~A"
		       item
		       items
		       1st-value
		       2nd-value)))))

(defun verify-name-and-arity-list (list)
  (when (every #'verify-name-and-arity-item list)
    (loop
       with already-seen-items = nil
       for item in list
       do
	 (let ((seen (member (car item) already-seen-items 
			     :test #'eql 
			     :key #'car)))
	   (if (null seen)
	       (push item already-seen-items)
	       (error 'duplicate-names-with-different-arities-error
		      :first-item (caar seen)
		      :first-value (cdar seen)
		      :second-item (car item)
		      :second-value (cdr item)
		      :list list)))
       finally
	 (return t))))

(defun make-infinite-variable-signature-with-equality 
    (&key constants predicates functions)
  "Make an infinite-variable first-order signature whose constants are
CONSTANTS, whose predicates are PREDICATEs, and whose functions are
FUNCTIONS.  All three arguments should be list (by default, they are
all NIL).  CONSTANTS will be regarded as a simple list (i.e., its
members will be treated as the signatures constants), whereas
PREDICATES and FUNCTIONS should be lists of cons cells (NAME . ARITY)
whose car will be treated as the name of the function/predicate whose
arity will be ARITY.  All three lists will be treated as sets; if
there are any duplicates in any of the lists, they will be removed.
The names of all the signature's elements should all be disjoint:
CONSTANTS should not overlap with (the cars of the elements of)
PREDICATES and FUNCTIONS, and (the cars of the elements of) PREDICATES
should not overlap with (the cars of the elements of) FUNCTIONS.  It
is an error if two names in PREDICATES or FUNCTIONS are equal (in the
sense of EQL) but with different corresponding arities."
  (let ((constants-no-dups (remove-duplicates constants :test #'eql))
	(predicates-no-dups (remove-duplicates predicates :test #'eql))
	(functions-no-dups (remove-duplicates functions :test #'eql)))
    (when (verify-name-and-arity-list predicates-no-dups)
      (when (verify-name-and-arity-list functions-no-dups)
	(make-instance 'infinite-variable-first-order-signature
		       :constants constants-no-dups
		       :predicates predicates-no-dups
		       :functions functions-no-dups)))))

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

(defun unary-predicates (signature)
  (predicates-of-arity signature 1))

(defun predicate-of-arity (signature relation-symbol arity)
  (some #'(lambda (symbol-and-arity)
	    (let ((symbol (car symbol-and-arity))
		  (num-args (cdr symbol-and-arity)))
	      (when (string= (symbol-name symbol)
			     (symbol-name relation-symbol))
		(= arity num-args))))
	(signature-predicates signature)))

(defun unary-predicate? (signature relation-symbol)
  (predicate-of-arity signature relation-symbol 1))

(defgeneric constant? (signature sym))
(defgeneric function? (signature sym))
(defgeneric predicate? (signature sym))

(defmethod constant? ((s signature) sym)
  (member sym (signature-constants s)))

(defmethod predicate? ((sig finite-variable-propositional-signature)
		       (pred-sym symbol))
  (member pred-sym (signature-predicates sig) 
	  :test #'string=
	  :key #'symbol-name))	  

(defmethod predicate? ((s first-order-signature) pred-sym)
  (some #'(lambda (sym) (eq sym pred-sym))
	(mapcar #'car (signature-predicates s))))

(defmethod function? ((s signature) func-sym)
  (some #'(lambda (sym) (eq sym func-sym))
	(mapcar #'car (signature-functions s))))

(defun valid-identifier-name? (str)
  (and str
       (not (empty-string? str))
       (not (contains-whitespace? str))))

(defmethod belongs-to-signature? ((sig finite-variable-first-order-signature)
				  (sym symbol))
  (or (constant? sig sym)
      (predicate? sig sym)
      (function? sig sym)))

(defmethod belongs-to-signature? ((sig infinite-variable-first-order-signature) 
				  (sym symbol))
  (or (constant? sig sym) 
      (predicate? sig sym)
      (function? sig sym)))

(defmethod belongs-to-signature? ((sig finite-variable-propositional-signature)
				  (sym symbol))
  (member sym (signature-predicates sig) :test #'eql))

(defmethod belongs-to-signature? ((sig signature) (str string))
  (belongs-to-signature? sig (symbolify-here str)))

(defmethod add-constant ((sig signature) (constant-sym symbol))
  (cond ((belongs-to-signature? sig constant-sym)
	 (error 'symbol-already-present-error
		:symbol constant-sym
		:signature sig))
	(t (push constant-sym (signature-constants sig))
	   sig)))

(defmethod add-constant ((sig signature) (constant-str string))
  (if (valid-identifier-name? constant-str)
      (add-constant sig (symbolify-here constant-str))
      (error 'unacceptable-identifier-name-error
	     :text constant-str)))

(defmethod delete-constant ((sig signature) (constant-sym symbol))
  (setf (signature-constants sig)
	(remove constant-sym (signature-constants sig))))

(defmethod delete-constant ((sig signature) (constant-str string))
  (delete-constant sig (symbolify-here constant-str)))

(defmethod add-predicate ((sig finite-variable-propositional-signature)
			  (pred-sym symbol)
			  arity)
  (declare (ignore arity))
  (push pred-sym (signature-predicates sig)))

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
      (add-predicate sig (symbolify-here pred-str) arity)
      (error 'unacceptable-identifier-name-error
	     :text pred-str)))

(defmethod delete-predicate ((sig finite-variable-propositional-signature)
			     (pred-sym symbol))
  (with-slots (predicates) sig
    (setf predicates (remove pred-sym predicates))))

(defmethod delete-predicate ((sig first-order-signature) (pred-sym symbol))
  (setf (signature-predicates sig)
	(remove (find pred-sym (signature-predicates sig) :key #'first)
		(signature-predicates sig))))

(defmethod delete-predicate ((sig signature) (pred-str string))
  (delete-predicate sig (symbolify-here pred-str)))

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
      (add-function sig (symbolify-here func-str) arity)
      (error 'unaccepptable-identifier-name-error
	     :symbol (symbolify-here func-str))))

(defmethod delete-function ((sig signature) func-sym)
  (setf (signature-functions sig)
	(remove (find func-sym (signature-functions sig) :key #'first)
		(signature-functions sig))))

(defmethod delete-function ((sig signature) (func-str string))
  (delete-function sig (symbolify-here func-str)))

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

;;; signature.lisp ends here