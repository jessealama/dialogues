;;; felscher.lisp W. Felscher's transformations between dialogues and proofs

(require 'utils "utils.lisp")
(require 'formulas "formulas.lisp")
(require 'figure "figure.lisp")

(defun axiom? (sequent)
  (let ((lhs (sequent-lhs sequent))
	(rhs (sequent-rhs sequent)))
    (or (contains-formula? lhs contradiction) ; ⊥ => anything, or A,X => A
	(and rhs (contains-formula? lhs (first rhs))))))

(defun figure-axiom? (figure)
  (eq (figure-label figure) 'axiom)) 

(defun lj-deduction (figure)
  "Determine whether FIGURE is an intuitionistic deduction."
  (declare (ignore figure))
  nil)

;; Transformations

(defun lj->lj-prime (figure)
  "Transform FIGURE, assumed to be an LJ deduction, into an LJ' deduction." 
  (if (figure-axiom? figure)
      figure
      (let ((rule (figure-label figure))
	    (content (figure-content figure))
	    (parents (figure-parents figure)))
	(if (eq rule 'right-implication)
	    (let ((rhs (sequent-rhs content))
		  (parent (first parents)))
	      (let ((implication (first rhs))
		    (parent-content (figure-content parent)))
		(let ((parent-lhs (sequent-lhs parent-content)))
		  (make-figure
		   :content content
		   :label 'right-implication-0
		   :parents (list
			     (make-figure :content
					  (make-sequent parent-lhs
							implication)
					  :label 'right-implication-1
					  :parents (mapcar #'lj->lj-prime parents)))))))
	    (make-figure :content content
			 :label rule
			 :parents (mapcar #'lj->lj-prime parents))))))

(defun lj-prime->lj (figure)
  "Transform FIGURE, assumed to be an LJ' deduction, into an LJ deduction."
  (declare (ignore figure))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant deduct-1
  (make-figure :content (make-sequent 'b (make-implication 'a 'b))
	       :label 'right-implication
	       :parents (list (make-figure :content (make-sequent (list 'a 'b) 'b)
					   :label 'axiom
					   :parents nil))))

;;; felscher.lisp ends here