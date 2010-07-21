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


(defgeneric propositional-formula-variables (formula))

(defmethod propositional-formula-variables ((formula atomic-formula))
  (list formula))

(defmethod propositional-formula-variables ((formula unary-connective-formula))
  (propositional-formula-variables (argument formula)))

(defmethod propositional-formula-variables ((formula binary-connective-formula))
  (remove-duplicates (append (propositional-formula-variables (lhs formula))
			     (propositional-formula-variables (rhs formula)))
		     :test #'equal-atomic-formulas?))

(defun isomorphic-propositional-formulas (formula-1 formula-2)
  (let ((vars-1 (propositional-formula-variables formula-1))
	(vars-2 (propositional-formula-variables formula-2)))
    (and (same-length vars-1 vars-2)
	 (let ((mgu (match-formulas formula-1 formula-2)))
	   (and (not (eq mgu :fail))
		(let ((full-mgu (complete-substitution mgu vars-1)))
		  (equal-sets? (substitution-range full-mgu)
			       vars-2
			       :test #'equal-atomic-formulas?)))))))

(defgeneric isomorphic-propositional-formulas? (formula-1 formula-2 &optional bindings))

(defmethod isomorphic-propositional-formulas? :around ((formula-1 formula)
						       (formula-2 formula)
						       &optional bindings)
  (declare (ignorable bindings))
  (let ((vars-1 (propositional-formula-variables formula-1))
	(vars-2 (propositional-formula-variables formula-2)))
    (if (same-length vars-1 vars-2)
	(call-next-method)
	:fail)))

(defmethod isomorphic-propositional-formulas? ((formula-1 atomic-formula)
					       (formula-2 atomic-formula)
					       &optional bindings)
  (let ((val-1 (assoc formula-1 bindings)))
    (if (null val-1)
	(acons formula-1 formula-2 bindings)
	(if (equal-formulas? (cdr val-1) formula-2)
	    bindings
	    :fail))))

(defmethod isomorphic-propositional-formulas? ((formula-1 atomic-formula)
					       (formula-2 composite-formula)
					       &optional bindings)
  (declare (ignore bindings))
  :fail)

(defmethod isomorphic-propositional-formulas? ((formula-1 unary-connective-formula)
					       (formula-2 atomic-formula)
					       &optional bindings)
  (declare (ignore bindings))
  :fail)

(defmethod isomorphic-propositional-formulas? ((formula-1 unary-connective-formula)
					       (formula-2 unary-connective-formula)
					       &optional bindings)
  (if (eql (class-of formula-1) (class-of formula-2))
      (isomorphic-propositional-formulas? (argument formula-1)
					  (argument formula-2)
					  bindings)
      :fail))

(defmethod isomorphic-propositional-formulas? ((formula-1 unary-connective-formula)
					       (formula-2 binary-connective-formula)
					       &optional bindings)
  (declare (ignore bindings))
  :fail)

(defmethod isomorphic-propositional-formulas? ((formula-1 binary-connective-formula)
					       (formula-2 atomic-formula)
					       &optional bindings)
  (declare (ignore bindings))
  :fail)

(defmethod isomorphic-propositional-formulas? ((formula-1 binary-connective-formula)
					       (formula-2 unary-connective-formula)
					       &optional bindings)
  (declare (ignore bindings))
  :fail)

(defmethod isomorphic-propositional-formulas? ((formula-1 binary-connective-formula)
					       (formula-2 binary-connective-formula)
					       &optional bindings)
  (if (eql (class-of formula-1) (class-of formula-2))
      (let ((iso-lhs (isomorphic-propositional-formulas? (lhs formula-1)
							 (lhs formula-2))))
	(if (eq iso-lhs :fail)
	    :fail
	    (if (compatible-substitutions? iso-lhs bindings)
		(isomorphic-propositional-formulas? (rhs formula-1)
						    (rhs formula-2)
						    (append iso-lhs bindings))
		:fail)))
      :fail))  

(defgeneric reduced-enumeration (depth signature)
  (:documentation "A list of all formulas of depth DEPTH from
SIGNATURE, modulo some notion of equivalence that depends on SIGNATURE."))

(let ((reduced-enumeration-table (make-hash-table :test #'equal)))
  (defmethod reduced-enumeration :around (depth (sig signature))
    (let* ((key (cons depth sig))
	   (old-val (gethash key reduced-enumeration-table)))
      (if (null old-val)
	  (let ((new-val (call-next-method)))
	    (setf (gethash key reduced-enumeration-table) new-val))
	  (progn
	    old-val)))))

(defun reduce-formula-list (lst)
  (remove-duplicates lst
		     :test #'(lambda (formula-1 formula-2)
			       (not (eq (isomorphic-propositional-formulas? formula-1
									     formula-2)
					:fail)))))

(defmethod reduced-enumeration (depth
				(sig finite-variable-propositional-signature))
  (let ((earlier-complete-values (make-array (list (1+ depth))
					     :initial-element nil))
	(earlier-reduced-values (make-array (list (1+ depth))
					    :initial-element nil)))
    (labels
	((ce (depth)
	   (cond ((minusp depth) nil)
		 ((zerop depth)
		  (let ((preds (mapcar #'make-atomic-formula
				       (signature-predicates sig))))
		    (when (null (aref earlier-complete-values 0))
		    (setf (aref earlier-complete-values 0) preds))
		  preds))
		 (t
		  (if (null (aref earlier-complete-values depth))
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
			     (setf (aref earlier-complete-values depth)
				   (append results
					   (mapcar #'negate (ce (1- depth)))))))
		      (aref earlier-complete-values depth)))))
       (re (depth)
	 (cond ((minusp depth) nil)
	       ((zerop depth)
		(let* ((preds (mapcar #'make-atomic-formula
				     (signature-predicates sig)))
		       (reduced-preds (reduce-formula-list preds)))
		  (when (null (aref earlier-reduced-values 0))
		    (setf (aref earlier-reduced-values 0) reduced-preds))))
	       ((null (aref earlier-reduced-values depth))
		(loop 
		   with sum-pairs = (pairs-summing-to (1- depth))
		   for (a . b) in sum-pairs
		   append (let ((ce-a (ce a))
				(ce-b (ce b)))
			    (append (reduce-formula-list
				     (apply-to-product #'make-binary-disjunction
						       ce-a ce-b))
				    (reduce-formula-list
				     (apply-to-product #'make-binary-conjunction
						       ce-a ce-b))
				    (reduce-formula-list
				     (apply-to-product #'make-implication
						       ce-a ce-b))))
		   into results
		   finally
		     (return
		       (setf (aref earlier-reduced-values depth)
			     (append results
				     (mapcar #'negate (re (1- depth)))))))))
	 (aref earlier-reduced-values depth)))
      (re depth))))
  
;;; enumerate.lisp ends here