;;; terms.lisp A representation of simple terms

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass term () nil)

(defclass function-term (term)
  ((function-symbol :initarg :function
		    :accessor function-symbol
		    :type string)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defclass variable-term (term)
  ((name :initarg :name
	 :accessor name
	 :type string)))

(defclass sorted-variable-term (term)
  ((name :initarg :name
	 :accessor name
	 :type string)
   (sort :initarg :sort
	 :accessor sort)))

(defgeneric equal-terms? (term-1 term-2))

(defmethod equal-terms? ((var-1 variable-term) (var-2 variable-term))
  (string= (name var-1) (name var-2)))

(defmethod equal-terms? ((var variable-term) (func-term function-term))
  nil)

(defmethod equal-terms? ((var-1 variable-term) (var-2 sorted-variable-term))
  nil)

(defmethod equal-terms? ((func-term function-term) (var variable-term))
  nil)

(defmethod equal-terms? ((func-term function-term) (var sorted-variable-term))
  nil)

(defmethod equal-terms? ((func-term-1 function-term) (func-term-2 function-term))
  (and (string= (function-symbol func-term-1) (function-symbol func-term-2))
       (every-pair #'equal-terms? (args func-term-1) (args func-term-2))))
		   
(defun make-variable (symbol-or-string)
  (let ((name (if (symbolp symbol-or-string)
			   (symbol-name symbol-or-string)
			   symbol-or-string)))
    (cond ((string= name "")
	   (error "One cannot make a variable with an empty name"))
	  (t (make-symbol (concatenate 'string "?" name))))))

(defgeneric term-in-signature? (term signature))

(defmethod term-in-signature? ((var variable-term) (sig signature))
  t)

(defmethod term-in-signature? ((func-term function-term) (sig signature))
  (let* ((func (function-symbol func-term))
	 (args (arguments func-term))
	 (num-args (length args)))
    (and (function-or-arity sig func num-args)
	 (every #'(lambda (arg)
		    (term-in-signature? arg signature))
		args))))

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

(defun read-term ()
  (let (response)
    (until (symbolp response)
      (setq response (read t nil nil)))
    response))

;;; terms.lisp ends here