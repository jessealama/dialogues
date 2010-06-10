;;; felscher-examples.lisp Some examples illustrating Felscher's transofmrations

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant deduct-1
  (make-figure :content (make-sequent 'q (make-implication 'p 'q))
	       :label 'right-implication
	       :parents (list 
			 (make-figure 
			  :content (make-sequent (list 'p 'q)
						 'p)
			  :label 'axiom
			  :parents nil))))

;;; felscher-examples.lisp ends here