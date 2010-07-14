;;; felscher.lisp W. Felscher's transformations between dialogues and proofs

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argumentation forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter rule-d01-alternating
  (make-offensive-rule
   :name "d01-alternating"
   :body (not (eq current-player
		  (move-player (nth-move dialogue current-reference))))
   :description "You cannot respond to your own statements."))

(defparameter rule-d01-conjunction
  (make-offensive-rule
   :name "d01-conjunction"
   :precondition (binary-conjunction? (nth-statement dialogue current-reference))
   :body (or (eq current-statement attack-left-conjunct)
	     (eq current-statement attack-right-conjunct))
   :description "Only two attacks against conjunctions are permitted: ATTACK-LEFT-CONJUNCT and ATTACK-RIGHT-CONJUNCT."))

(defparameter rule-d01-left-conjunct
  (make-offensive-rule
   :name "d01-left-conjunct"
   :precondition (eq current-statement attack-left-conjunct)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference))
	      (binary-conjunction? (nth-statement dialogue current-reference)))
   :description "One cannot attack the left conjunct of a formula that isn't a conjunction."))

(defparameter rule-d01-right-conjunct
  (make-offensive-rule
   :name "d01-right-conjunct"
   :precondition (eq current-statement attack-right-conjunct)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference))
	      (binary-conjunction? (nth-statement dialogue current-reference)))
   :description "One cannot attack the right conjunct of a formula that isn't a conjunction."))

(defparameter rule-d01-disjunction
  (make-offensive-rule
   :name "d01-disjunction"
   :precondition (binary-disjunction? (nth-statement dialogue current-reference))
   :body (eq current-statement which-disjunct?)
   :description "WHICH-DISJUNCT? is the only permissible attack against a disjunction."))

(defparameter rule-d01-which-disjunct
  (make-offensive-rule
   :name "d01-which-disjunct"
   :precondition (eq current-statement which-disjunct?)
   :body (binary-disjunction? (nth-statement dialogue current-reference))
   :description "The WHICH-DISJUNCT? attack applies only to disjunctions."))

(defparameter rule-d01-implication
  (make-offensive-rule
   :name "d01-implication"
   :precondition (implication? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement)
	      (equal-statements? current-statement
				 (antecedent
				  (nth-statement dialogue current-reference))))
   :description "To attack an implication, one must assert the antecdent."))

(defparameter rule-d01-negation
  (make-offensive-rule 
   :name "d01-negation"
   :precondition (negation? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement)
	      (equal-statements? current-statement
				 (unnegate
				  (nth-statement dialogue current-reference))))
   :description "To attack a negation, one must assert the \"unnegation\" of the negation."))

(defparameter rule-d01-universal
  (make-offensive-rule
   :name "d01-universal"
   :precondition (universal-generalization? (nth-statement dialogue current-reference))
   :body (non-symbolic-attack-term? current-statement)
   :description "To attack a universal, one must assert a term."))

(defparameter rule-d01-term
  (make-offensive-rule
   :name "d01-term"
   :precondition (non-symbolic-attack-term? current-statement)
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s)
		(universal-generalization? s)))
   :description "If one asserts a term as an attack, then the assertion being attacked must be a universal generalization."))

(defparameter rule-d01-which-instance
  (make-offensive-rule 
   :name "d01-which-instance"
   :precondition (eq current-statement which-instance?)
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s)
		(existential-generalization? s)))
   :description "The WHICH-INSTANCE? attack applies only to existential generalizations."))

(defparameter rule-d01-existential
  (make-offensive-rule
   :name "d01-existential"
   :precondition (existential-generalization? (nth-statement dialogue current-reference))
   :body (eq current-statement which-instance?)
   :description "WHICH-INSTANCE? is the only permissible attack on existential generalizations."))

(defparameter rule-d01-formula
  (make-offensive-rule 
   :name "d01-formula"
   :precondition (non-symbolic-attack-formula? current-statement)
   :body (or (implication? (nth-statement dialogue current-reference))
	     (negation? (nth-statement dialogue current-reference))
	     (universal-generalization? (nth-statement dialogue current-reference)))
   :description "When the attacking statement is a formula, the statement being attacked must be either an implication or a negation."))

(defparameter rule-d02-alternating
  (make-defensive-rule
   :name "d02-alternating"
   :body (let ((attack (nth-move dialogue current-reference)))
	   (when attack
	     (let ((attacking-player (move-player attack)))
	       (when attacking-player
		 (let ((attack-reference (move-reference attack)))
		   (when attack-reference
		     (let ((attacked-play (nth-move dialogue attack-reference)))
		       (when attacked-play
			 (let ((attacked-player (move-player attacked-play)))
			   (when attacked-player
			     (and (not (eq current-player attacking-player))
				  (eq current-player attacked-player))))))))))))
   :description "You can defend only against the other player's attacks, which themselves are supposed to be against your statements."))

(defparameter rule-d02-formula
  (make-defensive-rule
   :name "d02-formula"
   :body (non-symbolic-attack-formula? current-statement)
   :description "All defensive statements are supposed to be formulas."))

(defparameter rule-d02-left-conjunct
  (make-defensive-rule
   :name "d02-left-conjunct"
   :precondition (eq (nth-statement dialogue current-reference)
		  attack-left-conjunct)
  :body (with-original-statement (original-statement)
	  (equal-statements? current-statement
			     (lhs original-statement)))
  :description "To defend against the ATTACK-LEFT-CONJUNCT attack, assert the left conjunct of the original conjunction."))

(defparameter rule-d02-right-conjunct
  (make-defensive-rule 
   :name "d02-right-conjunct"
   :precondition (eq (nth-statement dialogue current-reference)
		  attack-right-conjunct)
   :body (with-original-statement (original-statement)
	   (equal-statements? current-statement
			      (rhs original-statement)))
   :description "To defend against the ATTACK-RIGHT-CONJUNCT attack, assert the right conjunct of the original conjunction."))

(defparameter rule-d02-which-disjunct
  (make-defensive-rule
   :name "d02-which-disjunct"
   :precondition (eq (nth-statement dialogue current-reference) which-disjunct?)
   :body (with-original-statement (original-statement)
	   (or (equal-statements? current-statement
				  (lhs original-statement))
	       (equal-statements? current-statement
				  (rhs original-statement))))
   :description "To defend against the WHICH-DISJUNCT? attack, assert either the left or the right disjunct of the original disjunction."))

(defparameter rule-d02-implication
  (make-defensive-rule 
   :name "d02-implication"
   :precondition (with-original-statement (original-statement)
		(implication? original-statement))
   :body (with-original-statement (original-statement)
	   (equal-statements? current-statement
			      (consequent original-statement)))
   :description "To defend against an attack on an implication, assert its consequent."))

(defparameter rule-d02-negation
  (make-defensive-rule 
   :name "d02-negation"
   :precondition (with-original-statement (original-statement)
		(negation? original-statement))
   :body nil
   :description "One cannot (directly) defend against an attack on a negation."))

(defparameter rule-d02-universal
  (make-defensive-rule
   :name "d02-universal"
   :precondition (with-original-statement (original-statement)
		(universal-generalization? original-statement))
   :body (let* ((attack (nth-move dialogue current-reference))
		(attack-refers-to (move-reference attack))
		(instance (move-statement attack))
		(original-move (nth-move dialogue attack-refers-to))
		(original-statement (move-statement original-move))
		(var (bound-variable original-statement))
		(matrix (matrix original-statement)))
	   (equal-statements? current-statement
			      (instantiate instance var matrix)))
   :description "The asserted statement is not the required instance of the original universal generalization."))

(defparameter rule-d02-existential
  (make-defensive-rule
   :name "d02-existential"
   :precondition (with-original-statement (original-statement)
		(existential-generalization? original-statement))
   :body (with-original-statement (original-statement)
	   (instance-of-quantified? current-statement
				    original-statement))
   :description "The asserted statement is not an instance of the original existential generalization."))

(defparameter argumentation-forms (list rule-d01-alternating
					rule-d01-conjunction
					rule-d01-left-conjunct
					rule-d01-right-conjunct
					rule-d01-disjunction
					rule-d01-which-disjunct
					rule-d01-implication
					rule-d01-negation
					;; rule-d01-universal
					;; rule-d01-term
					;; rule-d01-which-instance
					;; rule-d01-existential
					rule-d01-formula
					rule-d02-alternating
					rule-d02-formula
					rule-d02-left-conjunct
					rule-d02-right-conjunct
					rule-d02-which-disjunct
					rule-d02-implication
					rule-d02-negation
					;; rule-d02-universal
					;; rule-d02-existential
					))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter rule-d00-atomic
  (make-particle-rule :name "d00-atomic"
	     :precondition (zerop current-position)
	     :body (composite-formula? current-statement)
	     :description "Dialogues must open with a composite formula."))

(defparameter rule-d00-proponent
  (make-particle-rule :name "d00-proponent"
	     :precondition (evenp current-position)
	     :body (eq current-player 'p)
	     :description "Proponent plays even-numbered positions.  (Counting starts at zero.)"))

(defparameter rule-d00-opponent
  (make-particle-rule :name "d00-opponent"
	     :precondition (oddp current-position)
	     :body (eq current-player 'o)
	     :description "Opponent plays odd-numbered positions.  (Counting starts at zero.)"))

(defparameter rule-d01-composite
  (make-offensive-rule
   :name "d01-composite"
   :body (composite-formula? (nth-statement dialogue 
					    current-reference))
   :description "Atomic formulas cannot be attacked."))

(defparameter rule-d02-attack
  (make-defensive-rule 
   :name "d02-attack"
   :body (attacking-move? (nth-move dialogue current-reference))
   :description "The move being defended against is not an attack."))

(defparameter rule-d10
  (make-structural-rule 
   :name "d10"
   :description "Proponent cannot assert an atomic formula before opponent has asserted it."
  :predicate
  (loop
     with len = (dialogue-length dialogue)
     for move in (if final-move-only
		     (subseq (dialogue-plays dialogue) (1- len))
		     (dialogue-plays dialogue))
     for i from (if final-move-only
		    (1- len)
		    0)
     do
       (if (and (proponent-move? move)
		(atomic-formula? (move-statement move)))
	   (unless
	       (some-move
		#'(lambda (other-move)
		    (when (opponent-move? other-move)
		      (equal-statements? (move-statement other-move)
					 (move-statement move))))
		dialogue :end i)
	     (return nil)))
       finally (return t))))

(defparameter rule-d10-literal
  (make-structural-rule
   :name "d10-literal"
   :description "Proponent may assert an atomic formula only if Opponent has either asserted that formula, or its negation, earlier in the dialogue."
   :predicate
   (loop 
      with len = (dialogue-length dialogue)
      for move in (if final-move-only
		      (subseq (dialogue-plays dialogue) (1- len))
		      (dialogue-plays dialogue))
      for i from (if final-move-only
		     (1- len)
		     0)
      do
	(let ((statement (move-statement move)))
	  (if (and (proponent-move? move)
		   (atomic-formula? statement))
	    (unless
		(some-move
		 #'(lambda (other-move)
		     (let ((other-statement (move-statement other-move)))
		       (when (opponent-move? other-move)
			 (or (equal-statements? other-statement statement)
			     (and (negation? other-statement)
				  (equal-statements? (unnegate other-statement)
						     statement))))))
		 dialogue
		 :end i)
	      (return nil))))
      finally (return t))))

(defparameter rule-d11
  (make-structural-rule
   :name "d11"
   :description "You must defend against the most recent open attack."
   :predicate
   (loop
      with len = (dialogue-length dialogue)
      for move in (if final-move-only
		      (subseq (dialogue-plays dialogue) (1- len))
		      (dialogue-plays dialogue))
      for i from (if final-move-only
		     (1- len)
		     0)
      do
	(when (defensive-move? move)
	  (let ((most-recent (most-recent-open-attack dialogue :end i))
		(reference (move-reference move)))
	    (if (null most-recent)
		(return nil)
		(unless (= most-recent reference)
		  (return nil)))))
      finally (return t))))

(defparameter rule-d12
  (make-structural-rule
   :name "d12"
   :description "Attacks may be answered at most once."
   :predicate
   (loop 
      with len = (dialogue-length dialogue)
      for move in (if final-move-only
		      (subseq (dialogue-plays dialogue) (1- len))
		      (dialogue-plays dialogue))
      for i from (if final-move-only
		     (1- len)
		     0)
      do
	(when (attacking-move? move)
	  (unless (length-at-most (select-moves 
				   #'(lambda (other-move)
				       (and (defensive-move? other-move)
					    (= (move-reference other-move)
					       i)))
				   dialogue
				   :end i)
				  1)
	    (return nil)))
      finally (return t))))

(defparameter rule-d13
  (make-structural-rule
   :name "d13"
   :description "A P-assertion may be attacked at most once."
   :predicate
   (loop
      with len = (dialogue-length dialogue)
      for move in (if final-move-only
		      (subseq (dialogue-plays dialogue) (1- len))
		      (dialogue-plays dialogue))
      for i from (if final-move-only
		     (1- len)
		     0)
      do
	(when (proponent-move? move)
	  (unless (length-at-most (select-moves 
				   #'(lambda (other-move)
				       (and (attacking-move? other-move)
					    (= (move-reference other-move)
					       i)))
				   dialogue)
				  1)
	    (return nil)))
      finally (return t))))

(defparameter rule-d13-symmetric
  (make-structural-rule
   :name "d13-symmetric"
   :description "Assertions can be attacked at most once"
   :predicate
   (loop
      with len = (dialogue-length dialogue)
      for move in (if final-move-only
		      (subseq (dialogue-plays dialogue) (1- len))
		      (dialogue-plays dialogue))
      for i from (if final-move-only
		     (1- len)
		     0)
      do
	(unless (length-at-most (select-moves 
				 #'(lambda (other-move)
				     (and (attacking-move? other-move)
					  (= (move-reference other-move)
					     i)))
				 dialogue)
				1)
	  (return nil))
      finally (return t))))

(defparameter rule-e
   (make-structural-rule 
    :name "e"
    :description "Opponent must react to the most recent statement by Proponent."
    :predicate 
    (loop 
       with len = (dialogue-length dialogue)
       for move in (if final-move-only
		       (subseq (dialogue-plays dialogue) (1- len))
		       (dialogue-plays dialogue))
       for i from (if final-move-only
		      (1- len)
		      0)
       do
	 (when (opponent-move? move)
	   (let ((reference (move-reference move)))
	     (unless (= (1+ reference) i)
	       (return nil))))
       finally (return t))))

;; (defparameter rule-no-repetitions
;;   (make-structuralrule
;;    :name "no-repetitions"
;;    :precondition t
;;    :body (every-move #'(lambda (move)
;; 			 (or (initial-move? move)
;; 			     (not (eq (move-player move) current-player))
;; 			     (/= (move-reference move) current-reference)
;; 			     (not (eq (move-stance move) current-stance))
;; 			     (not (equal-statements? (move-statement move)
;; 						     current-statement))))
;; 		     dialogue)
;;    :description "You may not make the exact same move (same stance, same reference, same statement) twice."))

(defparameter d-dialogue-rules 
  (make-instance 'ruleset
		 :rules (append argumentation-forms 
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11
				      rule-d12
				      rule-d13))
		 :description "D rules (basic rules for intuitionistic logic)"))

(defparameter e-dialogue-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms 
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11
				      rule-d12
				      rule-d13
				      rule-e))
		 :description "E rules (D rules + Opponment must always respond to the immediately previous move)"))

(defparameter classical-dialogue-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms 
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      ; rule-d11
				      ; rule-d12
				      rule-d13
				      rule-e))
		 :description "Classical logic rules (drop Felscher's D11 and D12, but add rule E)"))

(defparameter nearly-classical-dialogue-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      ;; rule-d11
				      ;; rule-d12
				      rule-d13
				      ))
		 :description "Nearly (?) classical logic (drop Felscher's D11 and D12)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some variants of Felscher's rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter rule-d12-two-times
  (make-defensive-rule
   :name "d12"
   :body (every-move #'(lambda (move)
			 (or (initial-move? move)
			     (attacking-move? move)
			     (/= (move-reference move)
				 current-reference)))
		     dialogue)
   :description "Attacks may be answered at most twice."))

(defparameter rule-d13-two-times
  (make-offensive-rule
   :name "d13-two-times"
   :precondition (eq current-player 'o)
   :body (length-at-most (moves-referring-to dialogue current-reference) 2)
   :description "A P-assertion may be attacked at most twice."))

(defparameter rule-d13-three-times
  (make-offensive-rule
   :name "d13-three-times"
   :precondition (eq current-player 'o)
   :body (length-at-most (moves-referring-to dialogue current-reference) 3)
   :description "A P-assertion may be attacked at most twice."))

(defparameter d-dialogue-rules-literal-d10
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10-literal
				      rule-d11
				      rule-d12
				      rule-d13))
		 :description "D rules, but Proponent may assert an atomic formula only if Opponent has asserted the corresponding literal"))

(defparameter d-dialogue-rules-minus-d10
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10
				      rule-d11
				      rule-d12
				      rule-d13))
		 :description "D rules, but Proponent may assert atomic formulas at any time"))

(defparameter d-dialogue-rules-minus-d11
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      ;; rule-d11
				      rule-d12
				      rule-d13))
		 :description "D rules, but you may defend against any open attack"))

(defparameter d-dialogue-rules-minus-d12
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11
				      ;; rule-d12
				      rule-d13))
		 :description "D rules, but attacks may be answered any number of times"))

(defparameter d-dialogue-rules-symmetric-d13
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11
				      rule-d12
				      ;; rule-d13
				      rule-d13-symmetric))
		 :description "D rules, but attacks may be answered at most once"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Playing games with Felscher's rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play-d-dialogue-game (&optional signature initial-formula)
  (play-dialogue-game d-dialogue-rules signature initial-formula))

(defun play-e-dialogue-game (&optional signature initial-formula)
  (play-dialogue-game e-dialogue-rules signature initial-formula))

(defun play-classical-dialogue-game (&optional signature initial-formula)
  (play-dialogue-game classical-dialogue-rules signature initial-formula))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search for dialogues and for strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun d-dialogue-search-bfs (initial-statement signature)
  (dialogue-search-bfs d-dialogue-rules initial-statement signature))

(defun e-dialogue-search-bfs (initial-statement signature)
  (dialogue-search-bfs e-dialogue-rules initial-statement signature))

(defun d-dialogue-search-dfs (initial-statement signature)
  (dialogue-search-dfs d-dialogue-rules initial-statement signature))

(defun e-dialogue-search-dfs (initial-statement signature)
  (dialogue-search-dfs e-dialogue-rules initial-statement signature))

(defun d-dialogue-search-dfs-no-cycles (initial-statement signature)
  (dialogue-search-dfs-no-cycles d-dialogue-rules initial-statement signature))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Felscher's transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
			     (make-figure
			      :content
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

(defun proof-to-strategy (d)
  "Transform the deduction D with endformula x into a strategy for
  winning a dialogue game based on x."
  (declare (ignore d))
  nil)

;;; felscher.lisp ends here