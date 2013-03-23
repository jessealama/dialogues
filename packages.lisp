
(in-package :cl-user)

(defpackage :dialogues
  (:use :cl :alexandria)
  (:export ; utils
           #:EMPTY-STRING? ; funny: this has nothing to do with dialogues

	   #:PQRS-PROPOSITIONAL-SIGNATURE
	   #:MAKE-DIALOGUE
	   #:MOVE
	   #:MAKE-MOVE
	   #:DIALOGUE-PLAYS
	   #:EXTEND-DIALOGUE
	   #:ADD-MOVE-TO-DIALOGUE
	   #:EVALUATE-ALL-RULES
	   #:DIALOGUE-LENGTH
	   #:PARSE-FORMULA

	   ;; conditions
	   #:MALFORMED-FORMULA-ERROR
	   #:MALFORMED-FORMULA-ERROR-TEXT
	   #:MALFORMED-FORMULA-ERROR-SIGNATURE
	   #:NON-ATOMIC-FORMULA-ERROR
	   #:NON-ATOMIC-FORMULA-ERROR-TEXT
	   #:NON-ATOMIC-FORMULA-ERROR-SIGNATURE

	   ; rule sets
	   #:D-DIALOGUE-RULES

	   ;; slots

	   ; move class
	   #:PLAYER
	   #:STATEMENT
	   #:STANCE
	   #:REFERENCE

	   ; dialogue class
	   #:SIGNATURE
	   #:PLAYS
	   #:DIALOGUE-SIGNATURE

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
	   #:SMETANICH-FORMULA

	   ;; symbolic attacks
	   #:*ATTACK-LEFT-CONJUNCT*
	   #:*ATTACK-RIGHT-CONJUNCT*
	   #:*WHICH-INSTANCE?*
	   #:*WHICH-DISJUNCT?*

	   ;; server configuration
	   #:*DIALOGUE-SERVER-PORT*))

(defvar *dialogue-package* (find-package :dialogues))

;;; packages.lisp ends here
