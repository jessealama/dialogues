;;; utils.lisp For stuff that doesn't fit any where else

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro until (condition &body body)
  `(do nil (,condition) ,@body))

(defmacro while (condition &body body)
  `(do nil ((not ,condition)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-value-and-error ((value error) form &body body)
  `(multiple-value-bind (,value ,error)
       (ignore-errors (values ,form nil))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-array (lst)
  (if lst
      (let ((len (length lst)))
	(let ((a (make-array (list len))))
	  (do ((i 0 (1+ i))
	       (x (car lst) (car tail))
	       (tail (cdr lst) (cdr tail)))
	      ((null tail) a)
	    (setf (aref a i) x))))
      (make-array (list 0))))

(defun every-pair (pred lst-1 lst-2)
  "With LST-1 = (a-1 a-2 ...) and LST-2 = (b-1 b-2 ...), and PRED a
  binary predicate, determine whether PRED applies
  to (a-1,b-1), (a-2,b-2), ... . If LST-1 and LST-2 do not have the
  same length, return NIL."
  (if lst-1
      (when lst-2
	(do* ((head-1 (car lst-1) (car tail-1))
	      (head-2 (car lst-2) (car tail-2))
	      (tail-1 (cdr lst-1) (cdr tail-1))
	      (tail-2 (cdr lst-2) (cdr tail-2))
	      (pass (funcall pred head-1 head-2)
		    (funcall pred head-1 head-2)))
	     ((or (null tail-1)
		  (null tail-2)
		  (not pass)) (if (null tail-1)
				  (and (null tail-2) pass)
				  (not (null tail-2))))))
      (null lst-2)))

(defmacro push-all (objs lst)
  (let ((obj (gensym)))
    `(if (null ,objs)
	 ,lst
	 (dolist (,obj ,objs ,lst)
	   (push ,obj ,lst)))))

(defun every-pair? (pred lst-1 lst-2)
  (if lst-1
      (and lst-2
	   (funcall pred (car lst-1) (car lst-2))
	   (every-pair? pred (cdr lst-1) (cdr lst-2)))
      (null lst-2)))

(defun first-n (n lst)
  (loop
     for i from 1 upto n
     for elt in lst
     collect elt into result
     finally (return result)))

(defun map-initial-pairs (lst-1 lst-2 fn)
  (cond ((null lst-1)
	 (values 1 lst-2))
	((null lst-2)
	 (values 0 lst-1))
	(t
	 (funcall fn (car lst-1) (car lst-2))
	 (map-initial-pairs (cdr lst-1) (cdr lst-2) fn))))

(defun apply-to-product (fun lst-1 lst-2)
  (let (results)
    (dolist (elt-1 lst-1 results)
      (dolist (elt-2 lst-2)
	(push (funcall fun elt-1 elt-2) results)))))

(defun length-at-most (lst n)
  (cond ((minusp n)
	 t)
	((zerop n)
	 (null lst))
	(t
	 (length-at-most (cdr lst) (1- n)))))

(defun same-length (lst-1 lst-2)
  (if (null lst-1)
      (null lst-2)
      (if (null lst-2)
	  nil
	  (same-length (cdr lst-1) (cdr lst-2)))))

(defun first-n-satisfying (n pred lst)
  (cond ((minusp n) (values nil lst))
	((zerop n) (values nil lst))
	((null lst) (values nil nil))
	((funcall pred (car lst))
	 (multiple-value-bind (winners tail)
	     (first-n-satisfying (1- n) pred (cdr lst))
	   (values (cons (car lst) winners) (cdr tail))))
	(t
	 (first-n-satisfying n pred (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-parity (m n)
  (or (and (evenp m) (evenp n))
      (and (oddp m) (oddp n))))

(defun pairs-summing-to (n)
 "All pairs (A . B) of natural numbers summing to N."
 (loop
    for a from 0 upto n
    collect (cons a (- n a)) into pairs
    finally (return pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This could be done iteratively rather than recursively, but I'm not
;; sure it makes much of a difference.  I don't expect this function
;; to get called very much anyway.
(defun comma-separated-list (lst)
  (if lst
      (let ((head (car lst))
	    (tail (cdr lst)))
	(concatenate 'string
		     (if tail
			 (format nil "~A, " head)
			 (format nil "~A" head))
		     (comma-separated-list tail)))
      ""))

(defun read-symbol (&rest symbols)
  (let (response)
    (if symbols
	(until (and response
		    (symbolp response)
		    (member response symbols))
	  (setf response (read t nil nil)))
	(until (and response
		    (symbolp response))
	  (setf response (read t nil nil))))
    response))

(defmacro read-yes-or-no ()
  `(read-symbol 'y 'yes 'n 'no))

(defmacro with-yes-or-no (&key yes no)
  `(ecase (read-yes-or-no)
     (y ,@yes)
     (yes ,@yes)
     (n ,@no)
     (no ,@no)))

(defmacro yes-or-no-go (question prompt yes-tag no-tag)
  `(progn
     (msg "~A" ,question)
     (format t "~A" ,prompt)
     (ecase (read-yes-or-no)
       (y (go ,yes-tag))
       (yes (go ,yes-tag))
       (n (go ,no-tag))
       (no (go ,no-tag)))))

(defun read-symbol-different-from (&rest symbols)
  (let (response)
    (until (and response
		(not (member response symbols)))
      (setf response (read t nil nil)))
    response))

(defun read-number-in-interval (a b)
  (let (response)
    (until (and (numberp response)
		(<= a response)
		(<= response b))
      (setq response (read t nil nil)))
    response))

(defun read-non-negative-number-at-most (n)
  (read-number-in-interval 0 (1- n)))

(defun read-number-in-interval-or-symbol (m n &rest symbols)
  (let (response)
    (until (or (and (numberp response)
		    (<= m response)
		    (<= response n))
	       (and (symbolp response)
		    (member response symbols)))
      (setf response (read t nil nil)))
    response))

(defun read-natural-number ()
  (let (response)
    (until (and (integerp response)
		(<= 0 response))
      (setf response (read t nil nil)))
    response))

(defun read-positive-integer ()
  (let (response)
    (until (and (integerp response)
		(> 0 response))
      (setf response (read t nil nil)))
    response))

(defun msg (format-string &rest args)
  (apply #'format t (concatenate 'string format-string "~%") args))

(defmacro with-simple-prompt ((prompt) question &body body)
  `(progn
     (msg ,question)
     (format t "~A" ,prompt)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-string? (str)
  (when (stringp str)
    (string= str "")))

(defun contains-whitespace? (str)
  (cl-ppcre:scan "\\s" str))

(defun symbolify (str &optional (package *package*))
  (intern (string-upcase str) package))

(defun symbolify-here (str)
  (symbolify str (find-package :dialogues)))

;; (defun intern-in-dialogue-package (name)
;;   (intern name *dialogue-package*))

(defun concat-strings (&rest strings)
  (funcall #'concatenate
	   'string
	   strings))

(defun lex< (str-1 str-2)
  (cond ((string= str-1 "")
	 (not (string= str-2 "")))
	((string= str-2 "")
	 t)
	(t (let ((char-1 (char str-1 0))
		(char-2 (char str-2 0)))
	     (cond ((char< char-1 char-2)
		    t)
		   ((char= char-1 char-2)
		    (lex< (subseq str-1 1)
			  (subseq str-2 1)))
		   (t nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defconstant-if-unbound (var value &optional documentation)
  (if (null documentation)
      `(unless (boundp ',var)
	 (defconstant ,var ,value))
      (if (stringp documentation)
	  `(unless (boundp ',var)
	     (defconstant ,var ,value ,documentation))
	  (error "The third argument, if non-NIL, must be a string."))))

;;; utils.lisp ends here
