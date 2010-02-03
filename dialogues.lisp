;;; dialogues.lisp Play Lorenzen dialogue games

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'utils "utils.lisp")
  (require 'formulas "formulas.lisp"))

(defun print-move (move stream depth)
  (declare (ignore depth))
  (let ((stance (move-stance move))
	(ref (move-reference move))
	(statement (move-statement move)))
    (if (and stance ref) ; a non-initial move
	(format stream "[~A,~A] ~A" stance ref statement)
	(format stream "~A" statement))))

(defun make-move (player statement stance reference)
  (make-move-int :player player
		 :statement statement
		 :stance stance
		 :reference reference))

(defun make-proponent-move (statement stance reference)
  (make-move 'p statement stance reference))

(defun make-opponent-move (statement stance reference)
  (make-move 'o statement stance reference))

(defun make-attack (player statement reference)
  (make-move player statement 'a reference))

(defun make-defense (player statement reference)
  (make-move player statement 'd reference))

(defun proponent-move? (move)
  (eq (move-player move) 'p))

(defun opponent-move? (move)
  (eq (move-player move) 'o))

(defstruct (move
	     (:print-function print-move)
	     (:constructor make-move-int))
  player
  statement
  stance
  reference)

(defun attacking-move? (move)
  (eq (move-stance move) 'A))

(defun defensive-move? (move)
  (eq (move-stance move) 'D))

(defun print-move-at-position (position move stream)
  (let ((statement (move-statement move))
	(stance (move-stance move))
	(ref-index (move-reference move)))
    (if (evenp position)
	(if (zerop position)
	    (format stream "0 P ~A" statement)
	    (format stream "~A P [~A,~A] ~A" position
		                             stance
					     ref-index
					     statement))
	(format stream "~A O [~A,~A] ~A" position 
	                                 stance
					 ref-index
					 statement))))

(defun print-dialogue (dialogue stream depth)
  (declare (ignore depth))
  (let ((plays (dialogue-plays dialogue)))
    (when plays
      (do ((i 0 (1+ i))
	   (moves (cdr plays) (cdr moves))
	   (move (car plays) (car moves)))
	  ((null moves) (print-move-at-position i move stream))
	(print-move-at-position i move stream)
	(format stream "~%")))))

(defun make-dialogue (initial-statement)
  (let ((first-move (make-proponent-move initial-statement nil nil)))
    (make-dialogue-int :plays (list first-move)
		       :closed-attacks nil)))

(defun some-move (predicate dialogue)
  (some predicate (dialogue-plays dialogue)))

(defun every-move (predicate dialogue)
  (some predicate (dialogue-plays dialogue)))

(defun nth-move (dialogue n)
  (nth n (dialogue-plays dialogue)))

(defun last-move (dialogue)
  (nth-move dialogue (1- (length (dialogue-plays dialogue)))))

(defun nth-statement (dialogue n)
  (move-statement (nth-move dialogue n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argumentation forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe-adheres-to-argumentation-forms (move-1 move-2)
  "Determine whether MOVE-2 could follow MOVE-1 according to the
argumentation forms.

The argumentation forms are, strictly speaking, triples; it does not
make sense to say of, two moves, that they adhere to the
argumentation forms.  What is computed by this function is a derived
notion of adherence: MAYBE-ADHERES-TO-ARGUMENTATION-FORMS determines
whether there could exist a third move, say, MOVE-3, such
that (MOVE-1,MOVE-2,MOVE-3) adheres to the argumentation forms."
  (let ((statement-1 (move-statement move-1))
	(statement-2 (move-statement move-2))
	(player-1 (move-player move-1))
	(player-2 (move-player move-2))
	(stance (move-stance move-2)))
    (and (not (eq player-1 player-2))
	 (eq stance 'a)
	 (cond ((conjunction? statement-1)
		(or (eq statement-2 'attack-left-conjunct)
		    (eq statement-2 'attack-right-conjunct)))
	       ((disjunction? statement-1)
		(eq statement-2 'which-disjunct?))
	       ((implication? statement-1)
		(equal-formulas? statement-2 (antecedent statement-1)))
	       ((negation? statement-1)
		(equal-formulas? statement-2 (unnegate statement-1)))
	       ((universal? statement-1)
		(term? statement-2))
	       ((existential? statement-2)
		(eq statement-2 'which-instance?))
	       (t (error "Unrecognized statement: ~A" statement-1))))))

(defun adheres-to-argumentation-forms (move-1 move-2 move-3)
  "Determine whether the ordered triple (MOVE-1, MOVE-2, MOVE-3)
adheres to the argumentation forms."
  (let ((statement-1 (move-statement move-1))
	(statement-2 (move-statement move-2))
	(statement-3 (move-statement move-3))
	(player-1 (move-player move-1))
	(player-2 (move-player move-2))
	(player-3 (move-player move-3))
	(stance-2 (move-stance move-2))
	(stance-3 (move-stance move-3)))
    (and (eq player-1 player-3)
	 (not (eq player-1 player-2))
	 (eq stance-2 'a)
	 (eq stance-3 'd)
	 (cond ((conjunction? statement-1)
		(or (and (eq statement-2 'attack-left-conjunct)
			 (equal-formulas? statement-3
					  (left-conjunct statement-1)))
		    (and (eq statement-2 'attack-right-conjunct)
			 (equal-formulas? statement-3 
					  (right-conjunct statement-1)))))
	       ((disjunction? statement-1)
		(and (eq statement-2 'which-disjunct?)
		     (or (equal-formulas? statement-3 
					  (left-disjunct statement-1))
			 (equal-formulas? statement-3 
					  (right-conjunct statement-1)))))
	       ((implication? statement-1)
		(and (equal-formulas? statement-2 (antecedent statement-1))
		     (equal-formulas? statement-3 (consequent statement-1))))
	       ((negation? statement-1)
		(equal-formulas? statement-2 (unnegate statement-1)))
	       ((universal? statement-1)
		(when (term? statement-2)
		  (equal-formulas? statement-3 
				   (instantiate statement-2
						(bound-variable statement-1)
						(matrix statement-1)))))
	       ((existential? statement-2)
		(and (eq statement-2 'which-instance?)
		     (instance-of-quantified? statement-3 statement-1)))
	       (t (error "Unrecognized statement: ~A" statement-1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		      
(defmacro make-rule (&key name condition body failure-message)
  (let ((condition-result (gensym))
	(condition-error (gensym))
	(body-result (gensym))
	(body-error (gensym)))
  `(lambda (dialogue current-player 
	             current-position
	             current-statement
	             current-stance
	             current-reference)
     (declare (ignorable dialogue
			 current-player
			 current-position
			 current-statement
			 current-stance
			 current-reference))
     (with-value-and-error (,condition-result ,condition-error)
         ,condition
       (if ,condition-error
	   (warn "An error occured while evaluating the condition for rule ~A! The type of the error was ~A. Continuing..." 
		 (quote ,name)
		 ,condition-error)
	   (if ,condition-result
	       (with-value-and-error (,body-result ,body-error)
		   ,body
		 (if ,body-error 
		     (warn "An error occured while evaluating the body of rule ~A! The type of the error was ~A.  Continuing..."
			   (quote ,name)
			   ,body-error)
		     (values ,body-result (concatenate 'string "[~A] " ,failure-message (quote ,name)))))
	       (values t nil)))))))

(defvar rule-d00-atomic
  (make-rule :name d00-atomic
	     :condition (zerop current-position)
	     :body (composite-formula? current-statement)
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
  (make-rule :name d01-composite
	     :condition (eq current-stance 'a)
	     :body (composite-formula? (nth-statement dialogue 
						      current-reference))
	     :failure-message "Atomic formulas cannot be attacked."))

(defvar rule-d01-adheres-to-forms
  (make-rule :name d01-adherence
	     :condition (eq current-stance 'a)
	     :body (let ((attacking-move (make-move current-player
						    current-statement
						    current-stance
						    current-reference))
			 (attacked-move (nth-move dialogue current-reference)))
		     (maybe-adheres-to-argumentation-forms attacked-move
							   attacking-move))
	     :failure-message "The proposed attack does not adhere to the argumentation forms."))

(defvar rule-d02-attack
  (make-rule :name d02-attack
	     :condition (eq current-stance 'd)
	     :body (attacking-move? (nth-move dialogue current-reference))
	     :failure-message "The move being defended against is not an attack."))

(defvar rule-d02-adheres-to-forms
  (make-rule :name d02-adherence
	     :condition (eq current-stance 'd)
	     :body (let* ((middle-move (nth-move dialogue current-reference))
			  (first-move (nth-move dialogue 
						(move-reference middle-move)))
			  (proposed-move (make-move current-player
						    current-statement
						    current-stance
						    current-reference)))
		     (adheres-to-argumentation-forms first-move
						     middle-move
						     proposed-move))
	     :failure-message "The proposed move does not adhere to the argumentation forms."))

(defvar rule-d10
  (make-rule :name d10
	     :condition (and (evenp current-position) 
			     (atomic-formula? current-statement))
	     :body (some-move #'(lambda (move)
				  (when (opponent-move? move)
				    (equal-formulas? (move-statement move)
						     current-statement)))
			      dialogue)
	     :failure-message "Proponent cannot assert an atomic formula before opponent has asserted it."))

(defun closed-attack-indices (dialogue)
  (let ((defenses (remove-if #'attacking-move? (dialogue-plays dialogue))))
    (mapcar #'move-reference defenses)))

(defun open-attack-indices (dialogue)
  (let ((result nil)
	(moves (dialogue-plays dialogue)))
    (when moves
      (do ((i 0 (1+ i))
	   (move (car moves) (cdr moves-tail))
	   (moves-tail (cdr moves) (cdr moves-tail)))
	  ((null moves-tail) result)
	(if (attacking-move? move)
	    (push i result)
	    (setf result (delete (move-reference move) result)))))))

(defun most-recent-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (when open-attacks
      (car open-attacks))))

(defvar rule-d11
  (make-rule :name d11
	     :condition (eq current-stance 'd)
	     :body (= current-reference (most-recent-open-attack dialogue))
	     :failure-message "You must defend against only the most recent open attack."))

(defvar rule-d12
  (make-rule :name d12
	     :condition (eq current-stance 'd)
	     :body (every-move #'(lambda (move)
				   (or (attacking-move? move))
				       (/= (move-reference move)
					   current-reference))
			       dialogue)
	     :failure-message "Attacks may be answered at most once."))

(defvar rule-d13
  (make-rule :name d13
	     :condition (and (oddp current-position)
			     (eq current-stance 'a))
	     :body (every-move #'(lambda (move)
				   (or (proponent-move? move)
				       (/= (move-reference move)
					   current-reference)))
			       dialogue)
	     :failure-message "A P-assertion may be attacked at most once."))

(defvar rule-e
  (make-rule :name e
	     :condition (oddp current-position)
	     :body (= current-reference (1- current-position))
	     :failure-message "Opponent must react to the most recent statement by Proponent."))

(defvar d-dialogue-rules
  (list rule-d00-atomic
	rule-d00-proponent
	rule-d00-opponent
	rule-d01-composite
	rule-d01-adheres-to-forms
	rule-d02-attack
	rule-d02-adheres-to-forms
	rule-d10
	rule-d11
	rule-d12
	rule-d13))

(defvar e-dialogue-rules
  (append d-dialogue-rules (list rule-e)))
	
(defstruct (dialogue
	     (:print-function print-dialogue)
	     (:constructor make-dialogue-int))
  (plays nil :type list)
  (proponents-attacked-statements nil :type list)
  (defenses nil :type list)
  (closed-attacks nil :type list))

(defun dialogue-length (dialogue)
  (length (dialogue-plays dialogue)))

(defun dialogue? (d)
  "Determine whether the dialogue data structure D really does represent a Lorenzen dialogue."
  (let ((plays (dialogue-plays d)))
    (or (null plays)
	(let ((first (first plays)))
	  (let ((first-statement (move-statement first)))
	    (when (and (formula? first-statement)
		       (composite-formula? first-statement))
	      (let ((len (length plays)))
		(let ((plays-vec (list-to-array plays)))
		  (or (zerop len)
		      (do ((i 0 (1+ i))
			   (ok? t)
			   (move (aref plays-vec 0) (aref plays-vec i)))
			  ((and ok? (= i len)) ok?)
			(unless (= i 0)
			  (let ((ref (move-reference move)))
			    (unless (and (>= ref 0)
					 (< ref i)
					 (not (= (mod ref 2) (mod i 2)))
					 (or (attacking-move? move)
					     (let ((old-move (aref plays-vec ref)))
					       (attacking-move? old-move))))
			      (setq ok? nil))))))))))))))

(defun next-moves (dialogue)
  "The set of moves by which DIALOGUE can be legally extended."
  (declare (ignore dialogue))
  nil)

(defun proponent-wins? (dialogue)
  (let ((len (length (dialogue-plays dialogue))))
    (when (evenp len)
      (null (next-moves dialogue)))))

(defun add-move-to-dialogue (dialogue move)
  (setf (dialogue-plays dialogue)
	(append (dialogue-plays dialogue) 
		(list move))))

(defun previously-asserted? (dialogue statement)
  (member statement (dialogue-plays dialogue) :key #'move-statement :test #'equalp))

(defun opponent-already-attacked? (dialogue index-of-attack)
  (member index-of-attack (dialogue-proponents-attacked-statements dialogue)))

(defun close-attack (dialogue index-of-attack)
  (push index-of-attack (dialogue-closed-attacks dialogue)))

(defun register-attack-against-proponent (dialogue index-of-attacked-statement)
  (push index-of-attacked-statement (dialogue-proponents-attacked-statements dialogue)))

(defun register-defense (dialogue index-of-defense)
  (push index-of-defense (dialogue-defenses dialogue)))

(defun extend-dialogue-with-attack (dialogue rules)
  (declare (ignore rules))
  (let* ((index nil)
	 (turn-number (dialogue-length dialogue))
	 (current-player (if (evenp turn-number) 'p 'o)))
    (msg "Attack which statement? (Your response should be a number between 0 and ~A.) " (1- turn-number))
    (setq index (read-non-negative-number-at-most turn-number))
    (if (same-parity turn-number index)
	(msg "You cannot attack yourself!~%")
	(if (and (oddp turn-number) (opponent-already-attacked? dialogue index))
	    (msg "Opponent cannot attack move ~A because it has already been attacked.~%" index)
	    (let* ((attacked-move (nth-move dialogue turn-number))
		   (attacked-statement (move-statement attacked-move)))
	      (if (atomic-formula? attacked-statement)
		  (msg "One cannot attack atomic statements!~%")
		  (cond ((conjunction? attacked-statement)
			 (msg "The attacked statement,~%~%  ~A,~%~%is a conjunction. ")
			 (msg "Attack the left or right conjunct? " attacked-statement)
			 (let ((left-or-right (read-symbol 'l 'r)))
			   (msg "OK, you are attacking move ~A by challenging the other player to defend~%~%  ~A.~%" index
				   (if (eq left-or-right 'l)
				       (left-conjunct attacked-statement)
				       (right-conjunct attacked-statement)))
			   (add-move-to-dialogue dialogue
						 (if (eq left-or-right 'l)
						     (make-attack current-player 'attack-left-conjunct index)
						     (make-attack current-player 'attack-right-conjunct index)))
			   (if (oddp turn-number) (register-attack-against-proponent dialogue index))))
			((disjunction? attacked-statement)
			 (msg "OK, you are attacking move ~A by challenging the other player to specify one of the disjuncts and defend it.~%" index)
			 (add-move-to-dialogue dialogue (make-attack current-player 'which-disjunct? 'a))
			 (if (oddp turn-number) (register-attack-against-proponent dialogue index)))
			((implication? attacked-statement)
			 (let ((antecedent (antecedent attacked-statement))
			       (consequent (consequent attacked-statement)))
			   (if (and (evenp turn-number)
				    (atomic-formula? antecedent)
				    (not (previously-asserted? dialogue consequent)))
			       (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
			       (progn
				 (msg "OK, you are attacking move ~A by asserting~%~%  ~A~%~%and asking the other player to defend~%~%  ~A~%" index antecedent consequent)
				 (add-move-to-dialogue dialogue (make-attack current-player antecedent index))
				 (if (oddp turn-number) (register-attack-against-proponent dialogue index))))))
			((negation? attacked-statement)
			 (let ((unnegated (unnegate attacked-statement)))
			   (if (and (evenp turn-number)
				    (atomic-formula? unnegated)
				    (not (previously-asserted? dialogue unnegated)))
			       (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
			       (progn
				 (msg "OK, you are attacking move ~A by asserting ~A.~%" index unnegated)
				 (add-move-to-dialogue dialogue (make-attack current-player unnegated index))
				 (if (oddp turn-number) (register-attack-against-proponent dialogue index))))))
			((universal? attacked-statement)
			 (let ((var (bound-variable attacked-statement))
			       (formula (matrix attacked-statement))
			       (instance nil))
			   (msg "OK, you are attacking move ~A, a universal statement.~%" attacked-statement)
			   (msg "To attack a universal, you provide a term t; the other player must then defend the instantiated statement.~%.")
			   (msg "What term do you choose? (Your answer must be a symbol.) ")
			   (setq instance (read-term))
			   (msg "OK, the other player must now defend the statement ~A~%" (instantiate instance var formula))
			   (add-move-to-dialogue dialogue (make-attack current-player instance index))
			   (if (oddp turn-number) (register-attack-against-proponent dialogue index))))
			(t ;; existential case
			 (msg "OK, you are attacking move ~A, an existential statement.~%" attacked-statement)
			 (add-move-to-dialogue dialogue (make-attack current-player 'which-instance? index))
			 (if (oddp turn-number) (register-attack-against-proponent dialogue index))))))))))


(defun extend-dialogue-with-defense (dialogue rules)
  (declare (ignore rules))
  (let* ((index nil)
	 (turn-number (dialogue-length dialogue))
	 (current-player (if (evenp turn-number) 'p 'o)))
    (msg "Defend against which attack? (Your response should be a number between 0 and ~A.) " (1- turn-number))
    (setq index (read-non-negative-number-at-most turn-number))
    (if (same-parity turn-number index)
	(msg "One cannot attack oneself or defend against one's own attacks!~%")
	(if (member index (dialogue-closed-attacks dialogue))
	    (msg "Attack ~A has already been defended.~%" index)
	    (let* ((attacking-move (nth index (dialogue-plays dialogue)))
		   (attacking-statement (move-statement attacking-move))
		   (attack-refers-to (move-reference attacking-move)))
	      (if (member attack-refers-to (dialogue-closed-attacks dialogue))
		  (msg "The attack against which you are defending has already been responded to.~%")
		  (let* ((initial-move (nth attack-refers-to (dialogue-plays dialogue)))
			 (initial-statement (move-statement initial-move)))
		    (cond ((eq attacking-statement 'attack-left-conjunct)
			   (let ((left (left-conjunct initial-statement)))
			     (if (and (evenp turn-number)
				      (atomic-formula? left)
				      (not (previously-asserted? dialogue left)))
				 (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
				 (progn
				   (msg "OK, you are responding to the attack at position ~A by asserting ~A~%" index left)
				   (add-move-to-dialogue dialogue (make-defense current-player left index))
				   (close-attack dialogue index)))))
			  ((eq attacking-statement 'attack-right-conjunct)
			   (let ((right (right-conjunct initial-statement)))
			     (if (and (evenp turn-number)
				      (atomic-formula? right)
				      (not (previously-asserted? dialogue right)))
				 (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
				 (progn
				   (msg "OK, you are responding to the attack at position ~A by asserting ~A~%" index right)
				   (add-move-to-dialogue dialogue (make-defense  current-player right index))
				   (close-attack dialogue index)))))
			  ((eq attacking-statement 'which-disjunct?)
			   (let ((left-or-right nil)
				 (assertion nil))
			     (msg "You are responding to an attack on a disjunction:~%~%  ~A~%~%You must now choose either the left or the right disjunct.~%" initial-statement)
			     (setq left-or-right (read-symbol 'l 'r))
			     (setq assertion (if (eq left-or-right 'l)
						 (left-disjunct initial-statement)
						 (right-disjunct initial-statement)))
			     (msg "OK, you are responding to the attack by asserting ~A~%" assertion)
			     (add-move-to-dialogue dialogue (make-defense current-player assertion index))
			     (close-attack dialogue index)))
			  ((negation? initial-statement)
			   (msg "You cannot (directly) defend against an attack on a negation.~%"))
			  ((implication? initial-statement)
			   (let ((consequent (consequent initial-statement)))
			     (if (and (evenp turn-number)
				      (atomic-formula? consequent)
				      (not (previously-asserted? dialogue consequent)))
				 (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
				 (progn
				   (msg "OK, you are responding to the attack on the implication~%~%  ~A~%~%by asserting ~A.~%" initial-statement consequent)
				   (add-move-to-dialogue dialogue (make-defense current-player consequent index))
				   (close-attack dialogue index)))))
			  ((universal? initial-statement)
			   (let ((instantiated (instantiate attacking-statement (bound-variable initial-statement) (matrix initial-statement))))
			     (msg "OK, you are responding to the attack on your universal statement~%~%  ~A~%~%by asserting ~A"
				     initial-statement instantiated)
			     (add-move-to-dialogue dialogue (make-defense current-player instantiated index))
			     (close-attack dialogue index)))
			  ((existential? initial-statement)
			   (msg "OK, you are defending an existential claim.  To do so, you must provide a witness.~%")
			   (let ((witness nil))
			     (msg "What term do you choose? ")
			     (setf witness (read-term))
			     (let ((instantiated (instantiate witness (bound-variable initial-statement) (matrix initial-statement))))
			       (msg "OK, you've chosen term ~A. Your assertion for this move is thus~%~%  ~A~%" witness instantiated)
			       (add-move-to-dialogue dialogue (make-defense current-player instantiated index))
			       (close-attack dialogue index))))
			  (t (error "Unrecognized defensive move!"))))))))))

(defun evaluate-rules (rules dialogue turn-number statement stance)
  (if rules
      (do ((rule (car rules) (car rules-tail))
	   (rules-tail (cdr rules) (cdr rules-tail))
	   (message nil)
	   (all-ok? t))
	  ((null rules-tail) (values all-ok? message))
	  (multiple-value-bind (result error-message)
	      (funcall rule dialogue turn-number statement stance)
	    (unless result
	      (setf rules-tail nil
		    all-ok? nil
		    message error-message))))))

(defun play-dialogue-game (rules)
  (msg "Let's play a dialogue game!~%")
  (msg "Proponent starts by playing a composite formula.~%")
  (msg "Input a composite formula: ")
  (let* ((initial-statement (read-composite-formula))
	 (dialogue (make-dialogue initial-statement))
	 (turn-number 0)
	 (response nil))
    (until (eq response 'done)
      (msg "Turn number ~A: ~A's turn~%" turn-number
	                                 (if (evenp turn-number)
					     "Proponent"
					     "Opponent"))
      (msg "Attack (A), defend (D), print dialogue so far (P), or quit (Q)? ")
      (setf response (read-symbol 'a 'd 'p 'q))
      (ecase response
	(p (msg "~A" dialogue))
	(q (setf response 'done))
	(a (extend-dialogue-with-attack dialogue rules)
	   (incf turn-number))
	(d (extend-dialogue-with-defense dialogue rules)
	   (incf turn-number)))
      (msg "Thanks for playing.~%")
      (msg "The dialogue went like this:~%~A~%" dialogue))))

(defun play-d-dialogue-game ()
  (play-dialogue-game d-dialogue-rules))

(defun play-e-dialogue-game ()
  (play-dialogue-game e-dialogue-rules))

(defun play-as-proponent (formula)
  "Play a dialogue game for FORMULA as proponent."
  (declare (ignore formula))
  nil)

(defun play-as-opponent (formula)
  "Play a dialogue game for FORMULA as opponent."
  (declare (ignore formula))
  nil)

(provide 'dialogues)

;;; dialogues.lisp ends here