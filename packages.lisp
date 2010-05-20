
(in-package :cl-user)

(defpackage :dialogues
  (:use :cl)
  (:export ; utils
           #:EMPTY-STRING? ; funny: this has nothing to do with dialogues

	   #:PQRS-PROPOSITIONAL-SIGNATURE
	   #:MAKE-DIALOGUE
	   #:MAKE-MOVE
	   #:DIALOGUE-PLAYS
	   #:EXTEND-DIALOGUE
	   
	   ;; slots

	   ; move class
	   #:PLAYER
	   #:STATEMENT
	   #:STANCE
	   #:REFERENCE

	   ; dialogue class
	   #:SIGNATURE
	   #:PLAYS
	   
	   ; named formulas
	   #:PEIRCE-FORMULA
	   #:EXCLUDED-MIDDLE
	   #:DUMMETT-FORMULA
	   #:MARKOV-FORMULA
	   #:DOUBLE-NEGATION-INTRO
	   #:DOUBLE-NEGATION-ELIMINATION
	   #:K-FORMULA
	   #:B-FORMULA
	   #:C-FORMULA
	   #:W-FORMULA
	   #:WEAK-EXCLUDED-MIDDLE
	   #:SCOTT-FORMULA
	   #:SMETANICH-FORMULA))
