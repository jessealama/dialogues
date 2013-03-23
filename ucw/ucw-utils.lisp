
(in-package :dialogues-ucw)

;; Utils

(defun current-window () (context.window-component *context*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A proposal for handling conditions. See 
;;;
;;;  http://paste.lisp.org/display/111175
;;;
;;; for the original source.  I learned about this by engaging with
;;; unknown_lamer on irc.freenode.net#ucw
;;;
;;; Thanks, unknown_lamer.

(defun handle-action-condition (lambda &rest args)
  "Returns (condition-or-result condition-raised-p)"
  (handler-case (apply lambda args)
    (:no-error (&rest res)
      (values res nil))
    (t (c)
      (values c t))))

(defmacro ucw-handler-case ((fun &rest args) &body clauses)
  (let ((res (gensym))
	(errorp (gensym)))
    `(multiple-value-bind (,res ,errorp) (handle-action-condition #',fun ,@args)
       (if ,errorp
	   (typecase ,res
	     ,@(mapcar (lambda (clause)
			 (destructuring-bind (typespec lambda-list &rest forms)
			     clause
			   (if (eq typespec :no-error)
			       `(t (apply (lambda ,lambda-list ,@forms) ,res))
			       `(,typespec (let ,(when lambda-list
						       `((,(car lambda-list) ,res)))
					     ,@forms)))))
		       
		       clauses))
	   
	   (apply #'values ,res)))))

(defun html-quote (str)
  (<:as-is "&ldquo;") (<:as-html str) (<:as-is "&rdquo;"))

;;; ucw-utils.lisp ends here