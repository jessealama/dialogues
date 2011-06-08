;;; terms.lisp A representation of simple terms

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass term () nil)

(defun term? (thing)
  (typep thing 'term))

(defclass function-term (term)
  ((function-symbol :initarg :function
		    :accessor function-symbol
		    :type string)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defun make-function-term (function &rest args)
  (make-instance 'function-term
		 :function function
		 :args args))

(defclass variable-term (term)
  ((name :initarg :name
	 :accessor variable-name
	 :type string)))

(defun variable-term? (thing)
  (typep thing 'variable-term))

(defclass unsorted-variable (variable-term)
  nil)

(defun unsorted-variable? (thing)
  (typep thing 'unsorted-variable))

(defclass sorted-variable (variable-term)
  ((sort :initarg :sort
	 :accessor variable-sort)))

(defun sorted-variable? (thing)
  (typep thing 'sorted-variable))

(defgeneric make-unsorted-variable (source))

(defmethod make-unsorted-variable ((sym symbol))
  (let ((name (symbol-name sym)))
    (if (valid-identifier-name? name)
	(make-instance 'unsorted-variable
		       :name name)
	(error 'unacceptable-identifier-name-error
	       :text name))))

(defmethod make-unsorted-variable ((str string))
  (if (valid-identifier-name? str)
      (make-instance 'unsorted-variable
		     :name str)
      (error 'unacceptable-identifier-name-error
	     :text str)))

(defgeneric equal-variables? (var-1 var-2))

(defmethod equal-variables? ((var-1 unsorted-variable) (var-2 unsorted-variable))
  (string= (variable-name var-1)
	   (variable-name var-2)))

(defmethod equal-variables? ((var-1 unsorted-variable) (var-2 sorted-variable))
  nil)

(defmethod equal-variables? ((var-1 sorted-variable) (var-2 unsorted-variable))
  nil)

(defmethod equal-variables? ((var-1 sorted-variable) (var-2 sorted-variable))
  (and (string= (variable-name var-1)
		(variable-name var-2))
       (eql (variable-sort var-1)
	    (variable-sort var-2))))

(defgeneric equal-terms? (term-1 term-2))

(defmethod equal-terms? ((var-1 unsorted-variable) (var-2 unsorted-variable))
  (string= (variable-name var-1) (variable-name var-2)))

(defmethod equal-terms? ((var-1 unsorted-variable) (var-2 sorted-variable))
  nil)

(defmethod equal-terms? ((var unsorted-variable) (func-term function-term))
  nil)

(defmethod equal-terms? ((var-1 sorted-variable) (var-2 unsorted-variable))
  nil)

(defmethod equal-terms? ((var-1 sorted-variable) (var-2 sorted-variable))
  (string= (variable-name var-1) (variable-name var-2)))

(defmethod equal-terms? ((var sorted-variable) (func-term function-term))
  nil)

(defmethod equal-terms? ((func-term function-term) (var unsorted-variable))
  nil)

(defmethod equal-terms? ((func-term function-term) (var sorted-variable))
  nil)

(defmethod equal-terms? ((func-term-1 function-term) (func-term-2 function-term))
  (and (string= (function-symbol func-term-1) (function-symbol func-term-2))
       (every-pair #'equal-terms? (arguments func-term-1) 
		                  (arguments func-term-2))))
		   
(defun make-variable (symbol-or-string)
  (let ((name (if (symbolp symbol-or-string)
			   (symbol-name symbol-or-string)
			   symbol-or-string)))
    (cond ((string= name "")
	   (error "One cannot make a variable with an empty name"))
	  (t (make-symbol (concatenate 'string "?" name))))))

(defgeneric variable-in-signature? (signature thing))

(defmethod variable-in-signature? ((sig infinite-variable-first-order-signature)
				   (sym symbol))
  (let ((var (make-variable sym))
	(variable-tester (variable-test-pred sig)))
    (funcall variable-tester var)))    

(defmethod variable-in-signature? ((sig finite-variable-first-order-signature)
				   (sym symbol))
  (let ((var (make-variable sym)))
    (member var (signature-variables sig) :test #'equal-terms?)))

(defgeneric term-in-signature? (term signature))

(defmethod term-in-signature? ((var unsorted-variable) (sig signature))
  t)

(defmethod term-in-signature? ((var sorted-variable) (sig signature))
  (let ((sort (variable-sort var)))
    (unary-predicate? sig sort)))

(defmethod term-in-signature? ((func-term function-term) (sig signature))
  (let* ((func (function-symbol func-term))
	 (args (arguments func-term))
	 (num-args (length args)))
    (and (function-of-arity sig func num-args)
	 (every #'(lambda (arg)
		    (term-in-signature? arg sig))
		args))))

(defun read-unsorted-variable (&optional (prompt-stream *standard-output*)
			                 (prompt ">")
			                 (input-stream *standard-input*))
  (let (new-var proposed-name)
    (tagbody
       (go start)
     start
       (format prompt-stream "What is the name of the unsorted variable?  (It cannot be the empty string, nor can it have spaces in its name.)~%")
       (format prompt-stream "~A " prompt)
       (setf proposed-name (read-line input-stream))
       (cond ((empty-string? proposed-name)
	      (format prompt-stream "The empty string is not an acceptable name for a variable.  Please try again.~%")
	      (go start))
	     ((contains-whitespace? proposed-name)
	      (format prompt-stream "Variable names cannot contain whitespace characters.  Please try again.~%")
	      (go start))
	     (t
	      (setf new-var (make-instance 'unsorted-variable
					   :name proposed-name))
	      (go the-end)))
     the-end)
    new-var))

(defun read-sorted-variable-in-signature (signature &optional (prompt-stream *standard-output*)
			                                      (prompt ">")
			                                      (input-stream *standard-input*))
  (let ((unary-preds (unary-predicates signature)))
    (if (null unary-preds)
	nil
	(let ((num-preds (length unary-preds)))
	  (let (new-var proposed-sort proposed-name)
	    (tagbody 
	       (go start)
	     start
	       (format prompt-stream "Select the unary predicate from the signature that will be the sort of the new variable:~%")
	       (loop for i from 1 upto num-preds
		  for pred in unary-preds
		  do
		    (format prompt-stream "[~d] ~A~%" i pred))
	       (format prompt-stream "~A " prompt)
	       (let ((index (read-number-in-interval 1 num-preds)))
		 (setf proposed-sort (nth (1- index) unary-preds))
		 (format prompt-stream "What is the name of the variable of sort ~A?  (It cannot be the empty string, nor can it have spaces in its name.)~%" proposed-sort)
		 (format prompt-stream "~A " prompt)
		 (setf proposed-name (read-line input-stream))
		 (cond ((empty-string? proposed-name)
			(format prompt-stream "The empty string is not an acceptable name for a variable.  Please try again.~%")
			(go start))
		       ((contains-whitespace? proposed-name)
			(format prompt-stream "Variable names cannot contain whitespace characters.  Please try again.~%")
			(go start))
		       (t
			(setf new-var (make-instance 'sorted-variable
						     :name proposed-name
						     :sort proposed-sort))
			(go the-end))))
	     the-end)
	  new-var)))))

(defun read-function-term-in-signature (signature &optional (prompt-stream *standard-output*)
			                                    (prompt ">")
			                                    (input-stream *standard-input*))
  (let ((funcs (signature-functions signature)))
    (if (null funcs)
	nil
	(let (new-term proposed-function proposed-terms)
	  (tagbody
	     (go start)
	   start
	     (format prompt-stream "Select the function to be used in the function term:~%")
	     (loop with len = (length funcs)
		   for i from 1 upto len
		   for func-arity in funcs
		do
		  (destructuring-bind (name . arity)
		      func-arity
		    (format prompt-stream "[~d] ~A (of arity ~d)~%" i name arity)))
	     (format prompt-stream "~A " prompt)
	     (let ((index (read-number-in-interval 1 (length funcs))))
	       (let ((selected-func-and-arity (nth (1- index) funcs)))
		 (destructuring-bind (name . arity)
		     selected-func-and-arity
		   (setf proposed-function name)
		   (if (zerop arity)
		       (progn
			 (setf new-term (make-function-term name))
			 (go the-end))
		       (progn
			 (format prompt-stream "The function ~A has arity ~d, so you must now supply ~d terms as arguments for the new function symbols" name arity arity)
			 (loop for i from 1 upto arity
			    collect (read-term-in-signature signature prompt-stream prompt input-stream) into terms
			    do
			      (setf new-term (apply #'make-function-term name terms))
			    finally
			      (setf proposed-terms terms))
			 (go the-end))))))
	   the-end)
	  new-term))))
  

(defun read-term-in-signature (signature &optional (prompt-stream *standard-output*)
			                           (prompt ">")
			                           (input-stream *standard-input*))
  "Attempt to read a term that belongs to SIGNATURE.
PROMPT-STREAM (default *STANDARD-OUTPUT*) is the stream to which
prompts will be printed.  INPUT-STREAM (default *STANDARD-INPUT*) is
the stream from which user input will be read.  PROMPT is a string
that will be used as a prompt.  A space character will be printed
after PROMPT is printed."
  (let (new-term)
    (tagbody
       (go type-of-term)
     type-of-term
       (format prompt-stream "What type of term do you want to input? Enter:~%")
       (format prompt-stream "- U for an unsorted variable (you will then be asked to enter a name)~%")
       (format prompt-stream "- S for an sorted variable (you will then be asked to enter a name and a sort~%")
       (format prompt-stream "- F for an function term (you will then be asked to choose a function symbol from the signature and enter further terms as appropriate for the selected function's arity~%")
       (format prompt-stream "~A " prompt)
       (ecase (read-symbol 'u 's 'f)
	 (u (go read-unsorted-variable))
	 (s (go read-sorted-variable))
	 (f (go read-function-term)))
     read-unsorted-variable
       (setf new-term (read-unsorted-variable prompt-stream prompt input-stream))
       (go the-end)
     read-sorted-variable
       (setf new-term (read-sorted-variable-in-signature prompt-stream prompt input-stream))
       (go the-end)
     read-function-term
       (setf new-term (read-function-term-in-signature signature prompt-stream prompt input-stream))
       (go the-end)
     the-end)
  new-term))

;;; terms.lisp ends here