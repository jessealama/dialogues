;;; signature.lisp Propositional and first-order signatures

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

;;; signature.lisp ends here