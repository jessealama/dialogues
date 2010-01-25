;;; utils.lisp For stuff that doesn't fit any where else

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

(defmacro until (condition &body body)
  `(do nil (,condition) ,@body))

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

(defun read-symbol (&rest symbols)
  (let (response)
    (until (and (symbolp response)
		(member response symbols))
      (setq response (read t nil nil)))
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

(defun msg (format-string &rest args)
  (apply #'format t format-string args))

(defun same-parity (m n)
  (or (and (evenp m) (evenp n))
      (and (oddp m) (oddp n))))

(provide 'utils)

;;; utils.lisp ends here
