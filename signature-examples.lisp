;;; signature-examples.lisp Some concrete signatures

(in-package :dialogues)

(defconstant-if-unbound pqrs-propositional-signature
  (make-finite-variable-propositional-signature 'p 'q 'r 's))

(defconstant-if-unbound unary-pqrs-signature-with-equality
    (make-infinite-variable-signature-with-equality :predicates '((p . 1)
								  (q . 1)
								  (r . 1)
								  (s . 1))))

;;; signature-examples.lisp ends here