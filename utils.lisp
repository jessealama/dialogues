;;; utils.lisp For stuff that doesn't fit anywhere else

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro until (condition &body body)
  `(do nil (,condition) ,@body))

(defmacro while (condition &body body)
  `(do nil ((not ,condition)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-pair (pred lst-1 lst-2)
  "With LST-1 = (a-1 a-2 ...) and LST-2 = (b-1 b-2 ...), and PRED a
  binary predicate, determine whether PRED applies
  to (a-1,b-1), (a-2,b-2), ... . If LST-1 and LST-2 do not have the
  same length, return NIL."
  (if lst-1
      (when lst-2
	(when (funcall pred (first lst-1) (first lst-2))
	  (every-pair pred (rest lst-1) (rest lst-2))))
      (null lst-2)))

(defun first-n (n lst)
  (loop :for i :from 1 :upto n :for elt :in lst :collect elt))

(defun map-initial-pairs (lst-1 lst-2 fn)
  (cond ((null lst-1)
	 (values 1 lst-2))
	((null lst-2)
	 (values 0 lst-1))
	(t
	 (funcall fn (car lst-1) (car lst-2))
	 (map-initial-pairs (cdr lst-1) (cdr lst-2) fn))))

(defun length-at-most (lst n)
  (cond ((minusp n)
	 t)
	((zerop n)
	 (null lst))
	(t
	 (length-at-most (cdr lst) (1- n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-parity (m n)
  (or (and (evenp m) (evenp n))
      (and (oddp m) (oddp n))))

(defun pairs-summing-to (n)
 "All pairs (A . B) of natural numbers summing to N."
 (loop :for a :from 0 :upto n :collect (cons a (- n a))))

(defun parse-integer-noerror (x)
  (handler-case (parse-integer x :junk-allowed nil)
    (error () nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comma-separated-list (lst)
  (format nil "~{~a~^, ~}" lst))

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

(defun stringify (x)
  (format nil "~a" x))

(defun contains-whitespace? (str)
  (find-if #'(lambda (x)
	       (or (char= x #\Newline)
		   (char= x #\Tab)
		   (char= x #\Space)))
	   str))

(defun symbolify (str &optional (package *package*))
  (intern (string-upcase str) package))

(defun symbolify-here (str)
  (symbolify str (find-package :dialogues)))

(defun concat-strings (&rest strings)
  (funcall #'concatenate
	   'string
	   strings))

(defun lex< (str-1 str-2)
  (cond ((string= str-1 "")
	 (not (string= str-2 "")))
	((string= str-2 "")
	 nil)
	(t (let ((char-1 (char str-1 0))
		(char-2 (char str-2 0)))
	     (cond ((char< char-1 char-2)
		    t)
		   ((char= char-1 char-2)
		    (lex< (subseq str-1 1)
			  (subseq str-2 1)))
		   (t
		    nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-readable? (path)
  (and (probe-file path)
       (streamp (handler-case (open path :direction :probe)
		  (error () nil)))))

;;; utils.lisp ends here
