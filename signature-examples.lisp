;;; signature-examples.lisp Some concrete signatures

(in-package :dialogues)

(defparameter *alphabetic-propositional-signature*
  (make-finite-variable-propositional-signature
   'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'm
   'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))

;;; signature-examples.lisp ends here