;;; utils.lisp For stuff that doesn't fit any where else

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
    `(dolist (,obj ,objs)
       (push ,obj ,lst))))

(defun equal-length? (lst-1 lst-2)
  (if lst-1
      (and lst-2
	   (equal-length? (cdr lst-1) (cdr lst-2)))
      (null lst-2)))

(defun equal-sets? (set-1 set-2 &key test)
  (and (subsetp set-1 set-2 :test test)
       (subsetp set-2 set-1 :test test)))

(defun every-pair? (pred lst-1 lst-2)
  (if lst-1
      (and lst-2
	   (funcall pred (car lst-1) (car lst-2))
	   (every-pair? pred (cdr lst-1) (cdr lst-2)))
      (null lst-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-parity (m n)
  (or (and (evenp m) (evenp n))
      (and (oddp m) (oddp n))))

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
			 (format nil "~S, " head)
			 (format nil "~S" head))
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

(defun msg (format-string &rest args)
  (apply #'format t (concatenate 'string format-string "~%") args))

(defmacro with-simple-prompt ((prompt) question &body body)
  `(progn
     (msg ,question)
     (format t "~A" ,prompt)
     ,@body))

;;; utils.lisp ends here
