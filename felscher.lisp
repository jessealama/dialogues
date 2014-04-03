;;; felscher.lisp W. Felscher's transformations between dialogues and proofs

(in-package :dialogues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argumentation forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equal-players? (player-1 player-2)
  (string= (symbol-name player-1) (symbol-name player-2)))

(defparameter rule-d01-alternating
  (make-offensive-rule
   :name "d01-alternating"
   :body (not (equal-players? current-player
                              (move-player (nth-move dialogue current-reference))))
   :description "A player cannot respond to his own statements."))

(defparameter rule-d01-conjunction
  (make-offensive-rule
   :name "d01-conjunction"
   :precondition (binary-conjunction? (nth-statement dialogue current-reference))
   :body (or (eq current-statement *attack-left-conjunct*)
	     (eq current-statement *attack-right-conjunct*))
   :description "Only two attacks against conjunctions are permitted: ATTACK-LEFT-CONJUNCT and ATTACK-RIGHT-CONJUNCT."))

(defparameter rule-d01-left-conjunct
  (make-offensive-rule
   :name "d01-left-conjunct"
   :precondition (eq current-statement *attack-left-conjunct*)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference))
	      (binary-conjunction? (nth-statement dialogue current-reference)))
   :description "One cannot attack the left conjunct of a formula that isn't a conjunction."))

(defparameter rule-d01-right-conjunct
  (make-offensive-rule
   :name "d01-right-conjunct"
   :precondition (eq current-statement *attack-right-conjunct*)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference))
	      (binary-conjunction? (nth-statement dialogue current-reference)))
   :description "One cannot attack the right conjunct of a formula that isn't a conjunction."))

(defparameter rule-d01-disjunction
  (make-offensive-rule
   :name "d01-disjunction"
   :precondition (binary-disjunction? (nth-statement dialogue current-reference))
   :body (eq current-statement *which-disjunct?*)
   :description "WHICH-DISJUNCT? is the only permissible attack against a disjunction."))

(defparameter rule-d01-which-disjunct
  (make-offensive-rule
   :name "d01-which-disjunct"
   :precondition (eq current-statement *which-disjunct?*)
   :body (binary-disjunction? (nth-statement dialogue current-reference))
   :description "The WHICH-DISJUNCT? attack applies only to disjunctions."))

(defparameter rule-d01-implication
  (make-offensive-rule
   :name "d01-implication"
   :precondition (implication? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement)
	      (eq current-statement
		  (antecedent
		   (nth-statement dialogue current-reference))))
   :description "To attack an implication, one must assert the antecdent."))

(defparameter rule-d01-negation
  (make-offensive-rule
   :name "d01-negation"
   :precondition (negation? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement)
	      (eq current-statement
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
   :precondition (eq current-statement *which-instance?*)
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s)
		(existential-generalization? s)))
   :description "The WHICH-INSTANCE? attack applies only to existential generalizations."))

(defparameter rule-d01-existential
  (make-offensive-rule
   :name "d01-existential"
   :precondition (existential-generalization? (nth-statement dialogue current-reference))
   :body (eq current-statement *which-instance?*)
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
			     (and (not (equal-players? current-player
                                                       attacking-player))
				  (equal-players? current-player
                                                  attacked-player))))))))))))
   :description "A player X can defend only against the other player's attacks, which themselves are supposed to be against X's statements."))

(defparameter rule-d02-formula
  (make-defensive-rule
   :name "d02-formula"
   :body (non-symbolic-attack-formula? current-statement)
   :description "All defensive statements are supposed to be formulas."))

(defparameter rule-d02-left-conjunct
  (make-defensive-rule
   :name "d02-left-conjunct"
   :precondition (eq (nth-statement dialogue current-reference)
		     *attack-left-conjunct*)
  :body (with-original-statement (original-statement)
	  (eq current-statement
	      (lhs original-statement)))
  :description "To defend against the ATTACK-LEFT-CONJUNCT attack, assert the left conjunct of the original conjunction."))

(defparameter rule-d02-right-conjunct
  (make-defensive-rule
   :name "d02-right-conjunct"
   :precondition (eq (nth-statement dialogue current-reference)
		     *attack-right-conjunct*)
   :body (with-original-statement (original-statement)
	   (eq current-statement
	       (rhs original-statement)))
   :description "To defend against the ATTACK-RIGHT-CONJUNCT attack, assert the right conjunct of the original conjunction."))

(defparameter rule-d02-which-disjunct
  (make-defensive-rule
   :name "d02-which-disjunct"
   :precondition (eq (nth-statement dialogue current-reference)
		     *which-disjunct?*)
   :body (with-original-statement (original-statement)
	   (or (eq current-statement
		   (lhs original-statement))
	       (eq current-statement
		    (rhs original-statement))))
   :description "To defend against the WHICH-DISJUNCT? attack, assert either the left or the right disjunct of the original disjunction."))

(defparameter rule-d02-implication
  (make-defensive-rule
   :name "d02-implication"
   :precondition (with-original-statement (original-statement)
		(implication? original-statement))
   :body (with-original-statement (original-statement)
	   (eq current-statement
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
	   (eq current-statement
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
	     :body (string= (symbol-name current-player) "P")
	     :description "Proponent plays even-numbered positions.  (Counting starts at zero.)"))

(defparameter rule-d00-opponent
  (make-particle-rule :name "d00-opponent"
	     :precondition (oddp current-position)
	     :body (string= (symbol-name current-player) "O")
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
   :name "D10"
   :description "Proponent cannot assert an atomic formula before Opponent has asserted it."
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
   :name "D10-literal"
   :description "Proponent may assert an atom only if Opponent has earlier asserted either that atom or its negation."
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
			 (or (eq other-statement statement)
			     (and (negation? other-statement)
				  (eq (unnegate other-statement)
				      statement))))))
		 dialogue
		 :end i)
	      (return nil))))
      finally (return t))))

(defparameter rule-d11
  (make-structural-rule
   :name "D11"
   :description "A player must defend against the most recent open attack."
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

(defparameter rule-d11-most-recent-attack
  (make-structural-rule
   :name "D11-most-recent"
   :description "Defenses  must be against the most recent attack (open or closed)."
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
	  (let* ((player (move-player move))
		 (reference (move-reference move))
		 (other-player (other-player player))
		 (later-moves (subseq (dialogue-plays dialogue) (1+ reference) i))
		 (later-attacks-by-other-player (remove-if-not #'(lambda (other-move)
								   (and (eq (move-player other-move) other-player)
									(attacking-move? other-move)))
							       later-moves)))
	    (when later-attacks-by-other-player
	      (return nil))))
      finally (return t))))

(defparameter rule-d11-queue
  (make-structural-rule
   :name "D11-queue"
   :description "Defenses must be against the earliest open attack."
   :predicate
   (if final-move-only
       (let ((final-move (last-move dialogue)))
	 (if (defensive-move? final-move)
	     (= (move-reference final-move)
		(earliest-open-attack-by-player-excluding-move dialogue
							       (other-player
								(move-player final-move))
							       final-move))
	     t))
       (queue-ok dialogue))))

(defparameter rule-d11-proponent
  (make-structural-rule
   :name "D11-Proponent"
   :description "Proponent must defend against the most recent open attack."
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
	  (when (proponent-move? move)
	    (let ((most-recent (most-recent-open-attack dialogue :end i))
		  (reference (move-reference move)))
	      (if (null most-recent)
		  (return nil)
		  (unless (= most-recent reference)
		    (return nil))))))
      finally (return t))))

(defparameter rule-d11-opponent
  (make-structural-rule
   :name "D11-Opponent"
   :description "Opponent must defend against the most recent open attack."
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
	  (when (opponent-move? move)
	    (let ((most-recent (most-recent-open-attack dialogue :end i))
		  (reference (move-reference move)))
	      (if (null most-recent)
		  (return nil)
		  (unless (= most-recent reference)
		    (return nil))))))
      finally (return t))))

(defun earliest-open-attack-by-player-excluding-move (dialogue player move)
  (earliest-open-attack-for-player
   (truncate-dialogue dialogue
		      (position move (dialogue-plays dialogue)))
   player))

(defun queue-ok (dialogue)
  (every-defensive-move
   #'(lambda (defense)
       (= (move-reference defense)
	  (earliest-open-attack-by-player-excluding-move dialogue
							 (other-player
							  (move-player defense))
							 defense)))
   dialogue))

(defparameter rule-d12
  (make-structural-rule
   :name "D12"
   :description "Attacks may be answered at most once."
   :predicate
   (loop
      named outer-loop
      for move-1 in (dialogue-plays dialogue)
      do
	(when (defensive-move? move-1)
	  (loop
	     for move-2 in (dialogue-plays dialogue)
	     do
	       (when (defensive-move? move-2)
		 (unless (eq move-1 move-2)
		   (when (= (move-reference move-1)
			    (move-reference move-2))
		     (return-from outer-loop nil))))))
      finally
	(return-from outer-loop t))))

(defparameter rule-d13
  (make-structural-rule
   :name "D13"
   :description "Proponent's assertions may be attacked at most once."
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
   :name "Symmetric D13"
   :description "Assertions can be attacked at most once"
   :predicate
   (loop
      with len = (dialogue-length dialogue)
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
    :name "E"
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

(defparameter rule-e-implication-opponent
   (make-structural-rule
    :name "E-for-implications (Opponent)"
    :description "Opponent must react to the most recent statement by Proponent if that statement is an implication."
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
	 (when (and (opponent-move? move)
		    (> i 0))
	   (let ((previous-move (nth-move dialogue (1- i))))
	     (let ((other-formula (move-statement previous-move)))
	       (when (implication? other-formula)
		 (let ((reference (move-reference move)))
		   (unless (= (1+ reference) i)
		     (return nil)))))))
       finally (return t))))

(defparameter rule-e-implication-proponent
   (make-structural-rule
    :name "E-for-implications (Proponent)"
    :description "Proponent must react to the most recent statement by Opponent if that statement is an implication."
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
	 (when (and (proponent-move? move)
		    (> i 0))
	   (let ((previous-move (nth-move dialogue (1- i))))
	     (let ((other-formula (move-statement previous-move)))
	       (when (implication? other-formula)
		 (let ((reference (move-reference move)))
		   (unless (= (1+ reference) i)
		     (return nil)))))))
       finally (return t))))

(defparameter rule-no-repetitions
  (make-structural-rule
   :name "No repetitions"
   :description "Neither player may repeat moves (same stance, same reference, same statement)."
   :predicate
   (every-move #'(lambda (move)
		   (every-move #'(lambda (other-move) (or (eq move other-move)
							  (not (equal-moves? move
									     other-move))))
			       dialogue))
	       dialogue)))

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
		 :name "D"
		 :description "D10, D11, D12, and D13"))

(defparameter jesse-kludge
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list ;; rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10-literal
				      rule-d11
				      rule-d12
				      rule-d13))
		 :description "The Jesse Kludge Special"))

(defparameter d-dialogue-rules-queue
(make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11-queue
				      rule-d12
				      rule-d13))
		 :description "D rules (basic rules for intuitionistic logic) with queue-style D11"))

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
		 :name "E"
		 :description "D rules plus rule E"))

(defparameter e-dialogue-rules-queue
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
		 :description "E rules (D rules + Opponent must always respond to the immediately previous move), with queue-style D11"))

(defparameter skeletal-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack))
		 :name "Skeletal"
		 :description "Minimal structural rules"))

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
		 :name "CL"
		 :description "E rules minus D11 and D12"))

(defparameter classical-dialogue-rules-keiff
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
				      ; rule-e
				      rule-e-implication-proponent
				      rule-e-implication-opponent
				      ))
		 :name "CL á la Keiff"
		 :description "D rules minus D11 and D12, plus Keiff's variant of E"))

(defparameter conjectural-classical-dialogue-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11-most-recent-attack
				      ; rule-d12
				      rule-d13
				      rule-e))
		 :description "Conjectural classical logic rules (drop D11 and D12, add rule E, add most-recent-attack D11)"))

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
		 :name "N"
		 :description "D10 and D13"))

(defparameter sara-ad-hoc-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d12
				      rule-d13))
		 :description "D10, D12, D13"))

(defparameter sara-ad-hoc-rules-2
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      rule-d10
				      rule-d11
				      rule-d13))
		 :description "D10, D11, D13"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heuristic rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repetition-in-dialogue? (move dialogue)
  "Whether there is a move in DIALOGUE equal to MOVE (but not
  identical to it, in the sense of EQ)"
  (find-if #'(lambda (other-move)
		  (and (not (eq move other-move))
		       (equal-moves? move other-move)))
	   (dialogue-plays dialogue)))

(defparameter proponent-no-repeats
  (make-structural-rule
   :name "P-no-repeat"
   :description "Proponent cannot repeat moves"
   :predicate
   (every-proponent-move #'(lambda (pro-move)
			     (not (repetition-in-dialogue? pro-move dialogue)))
			 dialogue)))

(defun double-repetition-in-dialogue? (move dialogue)
  "Whether there is a move in DIALOGUE equal to MOVE (but not
  identical to it, in the sense of EQ)"
  (>= (count-if #'(lambda (other-move)
                    (and (not (eq move other-move))
                         (equal-moves? move other-move)))
                (dialogue-plays dialogue))
      2))

(defparameter proponent-no-duplicate-repeats
  (make-structural-rule
   :name "P-no-repeat-twice"
   :description "Proponent is allowed to repeat a move, but not twice."
   :predicate
   (every-proponent-move #'(lambda (pro-move)
                             (not (double-repetition-in-dialogue? pro-move dialogue)))
                         dialogue)))

(defparameter rule-d14
  (make-structural-rule
   :name "D14"
   :description "Proponent's attacks may be defended at most once."
   :predicate
   (every-opponent-move #'(lambda (move)
			    (or (attacking-move? move)
				(let ((ref (move-reference move)))
				  (every-opponent-move #'(lambda (other-move)
							   (let ((other-ref (move-reference other-move)))
							     (or (eq move other-move)
								 (attacking-move? other-move)
								 (/= ref other-ref))))
						       dialogue))))
			dialogue)))

(defparameter opponent-no-repeats
  (make-structural-rule
   :name "O-no-repeat"
   :description "Opponent cannot repeat moves"
   :predicate
   (every-opponent-move #'(lambda (opp-move)
			    (not (repetition-in-dialogue? opp-move dialogue)))
			dialogue)))

(defparameter lille-classical-dialogue-rules
  (make-instance 'ruleset
                 :rules (append argumentation-forms
                                (list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack)
                                (list opponent-no-repeats
                                      rule-d10))
                 :name "Lille"
                 :description "Conjectural Lille dialogue rules (particle rules, D10, and Opponent-no-repetition)"))

(defparameter e-dialogue-rules-no-pro-repetitions
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
				      rule-e
				      proponent-no-repeats))
		 :description "E rules (D rules + Opponent must always respond to the immediately previous move); Proponent repetitions disallowed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Minimal rulesets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter only-particle-rules
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10
				      ;; rule-d11
				      ;; rule-d12
				      ;; rule-d13
				      ))
		 :description "Only particle rules."))

(defparameter particle-rules+d10
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
				      ;; rule-d13
				      ))
		 :description "Only particle rules + D10."))

(defparameter particle-rules+d11
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10
				      rule-d11
				      ;; rule-d12
				      ;; rule-d13
				      ))
		 :description "Only particle rules + D11."))

(defparameter particle-rules+d12
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10
				      ;; rule-d11
				      rule-d12
				      ;; rule-d13
				      ))
		 :description "Only particle rules + D12."))

(defparameter particle-rules+d13
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10
				      ;; rule-d11
				      ;; rule-d12
				      rule-d13
				      ))
		 :description "Only particle rules + D13."))

(defparameter particle-rules+e
  (make-instance 'ruleset
		 :rules (append argumentation-forms
				(list rule-d00-atomic
				      rule-d00-proponent
				      rule-d00-opponent
				      rule-d01-composite
				      rule-d02-attack
				      ;; rule-d10
				      ;; rule-d11
				      ;; rule-d12
				      ;; rule-d13
				      rule-e
				      ))
		 :description "Only particle rules + E."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some variants of Felscher's rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter rule-d12-two-times
  (make-defensive-rule
   :name "D12 (2)"
   :body (every-move #'(lambda (move)
			 (or (initial-move? move)
			     (attacking-move? move)
			     (/= (move-reference move)
				 current-reference)))
		     dialogue)
   :description "Attacks may be answered at most twice."))

(defparameter rule-d13-two-times
  (make-offensive-rule
   :name "D13 (2)"
   :precondition (eq current-player 'o)
   :body (length-at-most (moves-referring-to dialogue current-reference) 2)
   :description "Proponent's assertions may be attacked at most two times."))

(defparameter rule-d13-three-times
  (make-offensive-rule
   :name "D13 (3)"
   :precondition (eq current-player 'o)
   :body (length-at-most (moves-referring-to dialogue current-reference) 3)
   :description "Proponent's assertions may be attacked at most three times."))

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

(defparameter e-dialogue-rules-literal-d10
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
				      rule-d13
				      rule-e))
		 :description "E rules, but Proponent may assert an atomic formula only if Opponent has asserted the corresponding literal"))

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
		 :description "D rules, but players may defend against any open attack (D11 absent)"))

(defparameter e-dialogue-rules-minus-d11
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
				      rule-d13
				      rule-e))
		 :description "E rules, but players may defend against any open attack (D11 absent)"))

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
		 :description "D rules, but attacks may be answered any number of times (D12 absent)"))

(defparameter e-dialogue-rules-minus-d12
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
				      rule-d13
				      rule-e))
		 :description "E rules, but attacks may be answered any number of times (D12 absent)"))

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

;;; felscher.lisp ends here
