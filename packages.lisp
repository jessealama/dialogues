
(in-package :cl-user)

(defpackage :dialogues
  (:use :cl :alexandria)
  (:export ; utils
           #:EMPTY-STRING? ; funny: this has nothing to do with dialogues

	   #:PQRS-PROPOSITIONAL-SIGNATURE
	   #:MAKE-DIALOGUE
	   #:MOVE
	   #:term
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
	   #:e-dialogue-rules
	   #:classical-dialogue-rules
	   #:nearly-classical-dialogue-rules
	   #:d-dialogue-rules-queue
	   #:e-dialogue-rules-queue
	   #:conjectural-classical-dialogue-rules
	   #:d-dialogue-rules-minus-d10
	   #:d-dialogue-rules-minus-d11
	   #:d-dialogue-rules-minus-d12
	   #:e-dialogue-rules-minus-d11
	   #:e-dialogue-rules-minus-d12
	   #:d-dialogue-rules-symmetric-d13
	   #:d-dialogue-rules-literal-d10
	   #:e-dialogue-rules-literal-d10
	   #:only-particle-rules
	   #:particle-rules+d10
	   #:particle-rules+d11
	   #:particle-rules+d12
	   #:particle-rules+d13
	   #:particle-rules+e
	   #:sara-ad-hoc-rules
	   #:sara-ad-hoc-rules-2

	   ;; slots
	   #:description

	   ; move class
	   #:PLAYER
	   #:STATEMENT
	   #:STANCE
	   #:REFERENCE

	   ; dialogue class
	   #:SIGNATURE
	   #:PLAYS
	   #:DIALOGUE-SIGNATURE

	   ;; formulas classes
	   #:unary-connective-formula
	   #:binary-connective-formula
	   #:binary-conjunction
	   #:binary-disjunction
	   #:implication
	   #:equivalence
	   #:negation
	   #:generalization
	   #:universal-generalization
	   #:existential-generalization
	   #:atomic-formula

	   #:lex<

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

	   ;; signatures
	   #:*ALPHABETIC-PROPOSITIONAL-SIGNATURE*

	   ;; translations
	   #:goedel-gentzen-translation
	   #:double-negate-translation
	   #:double-negate-all-subformulas-translation
	   #:kuroda-translation
	   #:negate-atomic-subformulas-translation
	   #:double-negate-atomic-subformulas-translation
	   #:self-conjoin-atomic-subformulas-translation
	   #:self-disjoin-atomic-subformulas-translation
	   #:contrapositivify-translation
	   #:contrapositive-translation
	   #:identity-translation
	   #:atomic->excluded-middle-translation
	   #:converse-translation

	   ;; formulas
	   #:peirce-formula
	   #:excluded-middle
	   #:weak-excluded-middle
	   #:conditional-excluded-middle
	   #:dummett-formula
	   #:double-negation-introduction
	   #:double-negation-elimination
	   #:k-formula
	   #:b-formula
	   #:c-formula
	   #:w-formula
	   #:i-formula
	   #:s-formula
	   #:scott-formula
	   #:smetanich-formula
	   #:de-morgan-not-and-implies-or-not
	   #:de-morgan-not-or-implies-and-not
	   #:de-morgan-and-not-implies-not-or
	   #:de-morgan-or-not-implies-not-and
	   #:anti-connexive-formula
	   #:ex-contradictione-quodlibet
	   #:implicational-ex-falso
	   #:kp
	   #:wkp
	   #:distributivity-of-implication-over-disjunction
	   #:aristotles-thesis-positive-antecedent
	   #:aristotles-thesis-negative-antecedent
	   #:modus-ponens
	   #:modus-tollens
	   #:hypothetical-syllogism
	   #:disjunctive-syllogism
	   #:constructive-dilemma
	   #:destructive-dilemma
	   #:conjunction-elimination-left
	   #:conjunction-elimination-right
	   #:conjunction-introduction
	   #:conjunction-introduction
	   #:disjunction-introduction-left
	   #:disjunction-introduction-right
	   #:composition
	   #:commutivity-of-conjunction
	   #:commutativity-of-disjunction
	   #:commutativity-of-implication
	   #:associativity-of-conjunction
	   #:associativity-of-disjunction
	   #:associativity-of-implication
	   #:distributivity-of-conjunction-over-disjunction-conjunctive-antecedent
	   #:distributivity-of-conjunction-over-disjunction-disjunctive-antecedent
	   #:distributivity-of-disjunction-over-conjunction-disjunctive-antecedent
	   #:distributivity-of-disjunction-over-conjunction-conjunctive-antecedent
	   #:transposition
	   #:material-implication-implicational-antecedent
	   #:material-implication-disjunctive-antecedent
	   #:material-implication-negative-antecedent
	   #:material-implication-negative-consequent
	   #:material-equivalence-conjunctive-antecedent
	   #:material-equivalence-disjunctive-antecedent
	   #:exportation-conjunctive-antecedent
	   #:exportation-implicational-antecedent
	   #:conjunctive-idempotency-conjunctive-antecedent
	   #:conjunctive-idempotency-conjunctive-consequent
	   #:disjunctive-idempotenency-disjunctive-antecedent
	   #:disjunctive-idempotenency-disjunctive-consequent
	   #:disjunctive-absorption-disjunctive-antecedent
	   #:disjunctive-absorption-disjunctive-consequent
	   #:conjunctive-absorption-conjunctive-antecedent
	   #:conjunctive-absorption-conjunctive-consequent
	   #:frege-formula
	   #:contraposition-positive-antecedent
	   #:contraposition-negative-antecedent
	   #:connexive-ax-1
	   #:connexive-ax-2
	   #:connexive-ax-3
	   #:connexive-ax-4
	   #:connexive-ax-5
	   #:connexive-ax-6
	   #:connexive-ax-7
	   #:connexive-ax-8
	   #:connexive-ax-9
	   #:connexive-ax-10
	   #:connexive-ax-11
	   #:connexive-ax-12
	   #:il-disjunction-formula
	   #:il-negation-formula

	   ;; constants
	   #:+strategy-max-depth+

	   ;; server configuration
	   #:*DIALOGUE-SERVER-PORT*))

(defvar *dialogue-package* (find-package :dialogues))

;;; packages.lisp ends here
