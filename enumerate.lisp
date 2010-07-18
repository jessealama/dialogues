;;; enumerate.lisp Systematically enumerate formulas

(in-package :dialogues)

(defgeneric complete-enumeration (depth signature))

(defmethod complete-enumeration (depth 
				 (sig finite-variable-propositional-signature))
  (let ((earlier-values (make-array (list (1+ depth))
				    :initial-element nil)))
    (labels
	((ce (depth)
	   (cond ((minusp depth) nil)
		 ((zerop depth)
		  (let ((preds (mapcar #'make-atomic-formula
				       (signature-predicates sig))))
		    (when (null (aref earlier-values 0))
		      (setf (aref earlier-values 0) preds))
		    preds))
		 (t
		  (if (null (aref earlier-values depth))
		      (loop 
			 with sum-pairs = (pairs-summing-to (1- depth))
			 for (a . b) in sum-pairs
			 append (let ((ce-a (ce a))
				      (ce-b (ce b)))
				  (append (apply-to-product #'make-binary-disjunction
							    ce-a ce-b)
					  (apply-to-product #'make-binary-conjunction
							    ce-a ce-b)
					  (apply-to-product #'make-implication
							    ce-a ce-b)))
			   into results
			 finally
			   (return
			     (setf (aref earlier-values depth)
				   (append results
					   (mapcar #'negate (ce (1- depth)))))))
		      (aref earlier-values depth))))))
      (ce depth))))


;;; enumerate.lisp ends here