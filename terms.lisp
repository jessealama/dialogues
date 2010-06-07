;;; terms.lisp A representation of simple terms

(in-package :dialogues)

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

(defun read-term ()
  (let (response)
    (until (symbolp response)
      (setq response (read t nil nil)))
    response))

;;; terms.lisp ends here