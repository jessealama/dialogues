
(in-package :cl-user)

(defpackage :dialogues
  (:use :cl :alexandria :yacc)
  (:import-from :cl-fad
		#:file-exists-p
		#:directory-exists-p
		#:directory-pathname-p
		#:pathname-as-directory)
  (:export ; utils
           #:EMPTY-STRING? ; funny: this has nothing to do with dialogues
	   #:concat-strings

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

	   ;; rules
	   #:rule-d10
	   #:rule-d10-literal
	   #:rule-d11
	   #:rule-d11-proponent
	   #:rule-d11-opponent
	   #:rule-d11-most-recent-attack
	   #:rule-d11-queue
	   #:rule-d12
	   #:rule-d12-two-times
	   #:rule-d13
	   #:rule-d13-symmetric
	   #:rule-d13-two-times
	   #:rule-d13-three-times
	   #:rule-d14
	   #:rule-e
	   #:rule-no-repetitions
	   #:opponent-no-repeats
	   #:proponent-no-repeats

	   ; rule sets
	   #:ruleset
	   #:copy-ruleset
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
	   #:skeletal-rules
	   #:classical-dialogue-rules-keiff
           #:lille-classical-dialogue-rules

	   ;; slots
	   #:dialogue-rules
	   #:description
	   #:lhs
	   #:rhs
	   #:arguments
	   #:argument
	   #:name
	   #:predicate
	   #:bound-variable
	   #:matrix
	   #:function-symbol
	   #:rules

	   ; move class
	   #:PLAYER
	   #:STATEMENT
	   #:STANCE
	   #:REFERENCE

	   ; dialogue class
	   #:SIGNATURE
	   #:PLAYS
	   #:DIALOGUE-SIGNATURE

	   ;; formulas classes and constructors
	   #:make-implication
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
	   #:uniquify-atoms

	   ;; symbolic attacks
	   #:*ATTACK-LEFT-CONJUNCT*
	   #:*ATTACK-RIGHT-CONJUNCT*
	   #:*WHICH-INSTANCE?*
	   #:*WHICH-DISJUNCT?*

	   ;; signatures
	   #:*ALPHABETIC-PROPOSITIONAL-SIGNATURE*
	   #:finite-variable-propositional-signature
	   #:belongs-to-signature?

	   ;; translations
	   #:apply-translation
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

	   ;; concrete formulas
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
	   #:implicational-ex-falso-variant
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

	   ;; moves
	   #:attacking-move?
	   #:initial-move?
	   #:move-statement
	   #:move-reference
	   #:last-move
	   #:move-<
	   #:move-stance
	   #:move-player
	   #:equal-moves?

	   ;; strategies
	   #:winning-strategy-for-opponent?
	   #:winning-strategy-for-proponent?
	   #:leaf-nodes
	   #:first-proponent-choice
	   #:first-opponent-choice
	   #:winning-strategy
	   #:node->strategy
	   #:first-splitting-descendant
	   #:open-in-every-branch?
	   #:node-successors
	   #:branch-closed?
	   #:proponent-wins-every-branch?
	   #:opponent-wins-every-branch?
	   #:first-splitter
	   #:first-splitting-descendant
	   #:closed-in-every-branch?
	   #:bounded-dialogue-search-bfs
           #:strategy-node
           #:strategy
           #:depth

	   ;; nodes and trees
	   #:root
	   #:node-state
	   #:node-depth
	   #:node-successors
	   #:expanded?
	   #:children
	   #:leaves

	   ;; utilities
	   #:until
	   #:empty-queue?
	   #:map-initial-pairs

	   ;; games
	   #:dialogue
	   #:open-attack-indices
	   #:initial-statement
	   #:eval-entire-dialogue
	   #:all-next-moves-at-position
	   #:copy-and-truncate-dialogue
	   #:next-attacks
	   #:next-defenses
	   #:eval-provisional-dialogue
	   #:add-defense-to-dialogue-at-position
	   #:add-attack-to-dialogue-at-position
	   #:copy-dialogue
           #:add-move-to-dialogue-at-position

	   #:render-plainly

	   ;; server configuration
	   #:*DIALOGUE-SERVER-PORT*))

(defvar *dialogue-package* (find-package :dialogues))

;;; packages.lisp ends here
