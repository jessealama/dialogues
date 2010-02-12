;;; felscher.lisp W. Felscher's transformations between dialogues and proofs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'utils "utils.lisp")
  (require 'formulas "formulas.lisp")
  (require 'figure "figure.lisp")
  (require 'dialogues "dialogues.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argumentation forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rule-d01-alternating
  (make-offensive-rule
   :name d01-alternating
   :body (not (eq current-player
		  (move-player (nth-move dialogue current-reference))))
   :failure-message "You cannot attack yourself."))

(defvar rule-d01-conjunction
  (make-offensive-rule
   :name d01-conjunction
   :condition (conjunction? (nth-statement dialogue current-reference))
   :body (or (eq current-statement 'attack-left-conjunct)
	     (eq current-statement 'attack-right-conjunct))
   :failure-message "Only two attacks against conjunctions are permitted:~%ATTACK-LEFT-CONJUNCT and ATTACK-RIGHT-CONJUNCT."))

(defvar rule-d01-left-conjunct
  (make-offensive-rule
   :name d01-left-conjunct
   :condition (eq current-statement 'attack-left-conjunct)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference)
	       (dialogue-signature dialogue))
	      (conjunction? (nth-statement dialogue current-reference)))
   :failure-message "One cannot attack the left conjunct of a formula~%that isn't a conjunction."))

(defvar rule-d01-right-conjunct
  (make-offensive-rule
   :name d01-right-conjunct
   :condition (eq current-statement 'attack-right-conjunct)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference)
	       (dialogue-signature dialogue))
	      (conjunction? (nth-statement dialogue current-reference)))
   :failure-message "One cannot attack the right conjunct of a formula that isn't a conjunction."))

(defvar rule-d01-disjunction
  (make-offensive-rule
   :name d01-disjunction
   :condition (disjunction? (nth-statement dialogue current-reference))
   :body (eq current-statement 'which-disjunct?)
   :failure-message "WHICH-DISJUNCT? is the only permissible attack against a disjunction."))

(defvar rule-d01-which-disjunct
  (make-offensive-rule
   :name d01-which-disjunct
   :condition (eq current-statement 'which-disjunct?)
   :body (disjunction? (nth-statement dialogue current-reference))
   :failure-message "The WHICH-DISJUNCT? attack applies only to disjunctions."))

(defvar rule-d01-implication
  (make-offensive-rule
   :name d01-implication
   :condition (implication? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
	      (equal-formulas? current-statement
			       (antecedent
				(nth-statement dialogue current-reference))))
   :failure-message "To attack an implication, one must assert the antecdent."))

(defvar rule-d01-negation
  (make-offensive-rule 
   :name d01-negation
   :condition (negation? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
	      (equal-formulas? current-statement
			       (unnegate
				(nth-statement dialogue current-reference))))
   :failure-message "To attack a negation, one must assert the \"unnegation\" of the negation."))

(defvar rule-d01-universal
  (make-offensive-rule
   :name d01-universal
   :condition (universal? (nth-statement dialogue current-reference))
   :body (non-symbolic-attack-term? current-statement
				    (dialogue-signature dialogue))
   :failure-message "To attack a universal, one must assert a term."))

(defvar rule-d01-term
  (make-offensive-rule
   :name d01-term
   :condition (non-symbolic-attack-term? current-statement
					 (dialogue-signature dialogue))
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s
					      (dialogue-signature dialogue))
		(universal? s)))
   :failure-message "If one asserts a term as an attack, then the assertion being attacked must be a universal generalization."))

(defvar rule-d01-which-instance
  (make-offensive-rule 
   :name d01-which-instance
   :condition (eq current-statement 'which-instance?)
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s
					      (dialogue-signature dialogue))
		(existential? s)))
   :failure-message "The WHICH-INSTANCE? attack applies only to existential generalizations."))

(defvar rule-d01-existential
  (make-offensive-rule
   :name d01-existential
   :condition (existential? (nth-statement dialogue current-reference))
   :body (eq current-statement 'which-instance?)
   :failure-message "WHICH-INSTANCE? is the only permissible attack on existential generalizations."))

(defvar rule-d01-formula
  (make-offensive-rule 
   :name d01-formula
   :condition (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
   :body (or (implication? (nth-statement dialogue current-reference))
	     (negation? (nth-statement dialogue current-reference))
	     (universal? (nth-statement dialogue current-reference)))
   :failure-message "When the attacking statement is a formula,~%the statement being attacked must be either~%an implication or a negation."))

(defvar rule-d02-alternating
  (make-defensive-rule
   :name d02-alternating
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
   :failure-message "You can defend only against the other player's attacks,~%which themselves are supposed to be against your statements."))

(defvar rule-d02-formula
  (make-defensive-rule
   :name d02-formula
   :body (non-symbolic-attack-formula? current-statement
				       (dialogue-signature dialogue))
   :failure-message "All defensive statements are supposed to be formulas."))

(defvar rule-d02-left-conjunct
  (make-defensive-rule
   :name d02-left-conjunct
   :condition (eq (nth-statement dialogue current-reference)
		  'attack-left-conjunct)
  :body (with-original-statement (original-statement)
	  (equal-formulas? current-statement
			   (left-conjunct original-statement)))
  :failure-message "To defend against the ATTACK-LEFT-CONJUNCT attack,~% assert the left conjunct of the original conjunction."))

(defvar rule-d02-right-conjunct
  (make-defensive-rule 
   :name d02-right-conjunct
   :condition (eq (nth-statement dialogue current-reference)
		  'attack-right-conjunct)
   :body (with-original-statement (original-statement)
	   (equal-formulas? current-statement
			    (right-conjunct original-statement)))
   :failure-message "To defend against the ATTACK-RIGHT-CONJUNCT attack,~% assert the right conjunct of the original conjunction."))

(defvar rule-d02-which-disjunct
  (make-defensive-rule
   :name d02-which-disjunct
   :condition (eq (nth-statement dialogue current-reference) 'which-disjunct?)
   :body (with-original-statement (original-statement)
	   (or (equal-formulas? current-statement
				(left-disjunct original-statement))
	       (equal-formulas? current-statement
				(right-disjunct original-statement))))
   :failure-message "To defend against the WHICH-DISJUNCT? attack,~%assert either the left or the right disjunct~%of the original disjunction."))

(defvar rule-d02-implication
  (make-defensive-rule 
   :name d02-implication
   :condition (with-original-statement (original-statement)
		(implication? original-statement))
   :body (with-original-statement (original-statement)
	   (equal-formulas? current-statement
			    (consequent original-statement)))
   :failure-message "To defend against an attack on an implication, assert its consequent."))

(defvar rule-d02-negation
  (make-defensive-rule 
   :name d02-negation
   :condition (with-original-statement (original-statement)
		(negation? original-statement))
   :body nil
   :failure-message "One cannot (directly) defend against an attack on a negation."))

(defvar rule-d02-universal
  (make-defensive-rule
   :name d02-universal
   :condition (with-original-statement (original-statement)
		(universal? original-statement))
   :body (let* ((attack (nth-move dialogue current-reference))
		(attack-refers-to (move-reference attack))
		(instance (move-statement attack))
		(original-move (nth-move dialogue attack-refers-to))
		(original-statement (move-statement original-move))
		(var (bound-variable original-statement))
		(matrix (matrix original-statement)))
	   (equal-formulas? current-statement
			    (instantiate instance var matrix)))
   :failure-message "The asserted statement is not the required instance of the original universal generalization."))

(defvar rule-d02-existential
  (make-defensive-rule
   :name d02-existential
   :condition (with-original-statement (original-statement)
		(existential? original-statement))
   :body (with-original-statement (original-statement)
	   (instance-of-quantified? current-statement
				    original-statement
				    (dialogue-signature dialogue)))
   :failure-message "The asserted statement is not an instance of the original existential generalization."))

(defvar argumentation-forms (list rule-d01-alternating
				  rule-d01-conjunction
				  rule-d01-left-conjunct
				  rule-d01-right-conjunct
				  rule-d01-disjunction
				  rule-d01-which-disjunct
				  rule-d01-implication
				  rule-d01-negation
				  rule-d01-universal
				  rule-d01-term
				  rule-d01-which-instance
				  rule-d01-existential
				  rule-d01-formula
				  rule-d02-alternating
				  rule-d02-formula
				  rule-d02-left-conjunct
				  rule-d02-right-conjunct
				  rule-d02-which-disjunct
				  rule-d02-implication
				  rule-d02-negation
				  rule-d02-universal
				  rule-d02-existential))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rule-d00-atomic
  (make-rule :name d00-atomic
	     :condition (zerop current-position)
	     :body (composite-formula? current-statement
				       (dialogue-signature dialogue))
	     :failure-message "Dialogues must open with a composite formula."))

(defvar rule-d00-proponent
  (make-rule :name d00-proponent
	     :condition (evenp current-position)
	     :body (eq current-player 'p)
	     :failure-message "Proponent plays even-numbered positions.  (Counting starts at zero.)"))

(defvar rule-d00-opponent
  (make-rule :name d00-opponent
	     :condition (oddp current-position)
	     :body (eq current-player 'o)
	     :failure-message "Opponent plays odd-numbered positions.  (Counting starts at zero.)"))

(defvar rule-d01-composite
  (make-offensive-rule
   :name d01-composite
   :body (composite-formula? (nth-statement dialogue 
					    current-reference)
			     (dialogue-signature dialogue))
   :failure-message "Atomic formulas cannot be attacked."))

(defvar rule-d02-attack
  (make-defensive-rule 
   :name d02-attack
   :body (attacking-move? (nth-move dialogue current-reference))
   :failure-message "The move being defended against is not an attack."))

(defvar rule-d10
  (make-rule :name d10
	     :condition (and (evenp current-position) 
			     (non-symbolic-attack-formula? current-statement
							   (dialogue-signature dialogue))
			     (atomic-formula? current-statement
					      (dialogue-signature dialogue)))
	     :body (some-move #'(lambda (move)
				  (when (opponent-move? move)
				    (equal-formulas? (move-statement move)
						     current-statement)))
			      dialogue)
	     :failure-message "Proponent cannot assert an atomic formula before opponent has asserted it."))

(defvar rule-d11
  (make-defensive-rule 
   :name d11
   :body (let ((most-recent (most-recent-open-attack dialogue)))
	   (or (null most-recent)
	       (= most-recent current-reference)))
   :failure-message "You must defend against only the most recent open attack."))

(defvar rule-d12
  (make-defensive-rule
   :name d12
   :body (every-move #'(lambda (move)
			 (or (initial-move? move)
			     (attacking-move? move)
			     (/= (move-reference move)
				 current-reference)))
		     dialogue)
   :failure-message "Attacks may be answered at most once."))

(defvar rule-d13
  (make-offensive-rule
   :name d13
   :condition (eq current-player 'o)
   :body (every-move #'(lambda (move)
			 (or (proponent-move? move)
			     (/= (move-reference move)
				 current-reference)))
		     dialogue)
   :failure-message "A P-assertion may be attacked at most once."))

(defvar rule-e
  (make-rule :name e
	     :condition (eq current-player 'o)
	     :body (= current-reference (1- current-position))
	     :failure-message "Opponent must react to the most recent statement by Proponent."))

(defvar d-dialogue-rules (append argumentation-forms 
				 (list rule-d00-atomic
				       rule-d00-proponent
				       rule-d00-opponent
				       rule-d01-composite
				       rule-d02-attack
				       rule-d10
				       rule-d11
				       rule-d12
				       rule-d13)))

(defvar e-dialogue-rules (append d-dialogue-rules (list rule-e)))

(defvar classical-dialogue-rules
  (append argumentation-forms
	  (list rule-d00-atomic
		rule-d00-proponent
		rule-d00-opponent
		rule-d01-composite
		rule-d02-attack
		rule-d10
		;; rule-d11
		;; rule-d12
		rule-d13)))

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
			     (make-figure :content
					  (make-sequent pqrs-signature
							parent-lhs
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter deduct-1
  (make-figure :content (make-sequent pqrs-signature
				      'q (make-implication 'p 'q))
	       :label 'right-implication
	       :parents (list 
			 (make-figure :content (make-sequent pqrs-signature
							     (list 'p 'q)
							     'p)
				      :label 'axiom
				      :parents nil))))

;;; felscher.lisp ends here