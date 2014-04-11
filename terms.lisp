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

;;; terms.lisp ends here
