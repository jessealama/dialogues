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

(provide 'utils)

;;; utils.lisp ends here
