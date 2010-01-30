;;; dialogues.lisp Play Lorenzen dialogue games

(require 'utils "utils.lisp")
(require 'formulas "formulas.lisp")

(defun print-move (move stream depth)
  (declare (ignore depth))
  (let ((stance (move-stance move))
	(ref (move-reference move))
	(statement (move-statement move)))
    (if (and stance ref) ; a non-initial move
	(format stream "[~A,~A] ~A" stance ref statement)
	(format stream "~A" statement))))

(defun make-move (statement stance reference)
  (make-move-int :statement statement
		 :stance stance
		 :reference reference))

(defun make-attack (statement reference)
  (make-move statement 'a reference))

(defun make-defense (statement reference)
  (make-move statement 'd reference))

(defstruct (move
	     (:print-function print-move)
	     (:constructor make-move-int))
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
  (let ((first-move (make-move initial-statement nil nil)))
    (make-dialogue-int :plays (list first-move)
		       :length 1
		       :closed-attacks nil)))


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
		       (not (atomic-formula? first-statement)))
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

(defun nth-move (dialogue n)
  (nth n (dialogue-plays dialogue)))

(defun register-defense (dialogue index-of-defense)
  (push index-of-defense (dialogue-defenses dialogue)))

(defun extend-dialogue-with-attack (dialogue)
  (let ((index nil)
	(turn-number (dialogue-length dialogue)))
    (msg "Attack which statement? (Your response should be a number between 0 and ~A.) " (1- turn-number))
    (setq index (read-non-negative-number-at-most turn-number))
    (if (same-parity turn-number index)
	(msg "One cannot attack oneself or defend against one's own attacks!~%")
	(if (and (oddp turn-number) (opponent-already-attacked? dialogue index))
	    (msg "Opponent cannot attack move ~A because it has already been attacked.~%" index)
	    (let* ((attacked-move (nth-move dialogue n))
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
						     (make-attack 'attack-left-conjunction index)
						     (make-attack 'attack-right-conjunction index)))
			   (if (oddp turn-number) (register-attack-against-proponent dialogue index))))
			((disjunction? attacked-statement)
			 (msg "OK, you are attacking move ~A by challenging the other player to specify one of the disjuncts and defend it.~%" index)
			 (add-move-to-dialogue dialogue (make-attack 'which-disjunct? 'a))
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
				 (add-move-to-dialogue dialogue (make-attack antecedent index))
				 (if (oddp turn-number) (register-attack-against-proponent dialogue index))))))
			((negation? attacked-statement)
			 (let ((unnegated (unnegate attacked-statement)))
			   (if (and (evenp turn-number)
				    (atomic-formula? unnegated)
				    (not (previously-asserted? dialogue unnegated)))
			       (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
			       (progn
				 (msg "OK, you are attacking move ~A by asserting ~A.~%" index unnegated)
				 (add-move-to-dialogue dialogue (make-attack unnegated index))
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
			   (add-move-to-dialogue dialogue (make-attack instance index))
			   (if (oddp turn-number) (register-attack-against-proponent dialogue index))))
			(t ;; existential case
			 (msg "OK, you are attacking move ~A, an existential statement.~%" attacked-statement)
			 (add-move-to-dialogue dialogue (make-attack 'which-instance? index))
			 (if (oddp turn-number) (register-attack-against-proponent dialogue index))))))))))


(defun extend-dialogue-with-defense (dialogue)
  (let ((index nil)
	(turn-number (dialogue-length dialogue)))
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
		    (cond ((eq attacking-statement 'attack-left-conjunction)
			   (let ((left (left-conjunct initial-statement)))
			     (if (and (evenp turn-number)
				      (atomic-formula? left)
				      (not (previously-asserted? dialogue left)))
				 (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
				 (progn
				   (msg "OK, you are responding to the attack at position ~A by asserting ~A~%" index left)
				   (add-move-to-dialogue dialogue (make-defense left index))
				   (close-attack dialogue index)))))
			  ((eq attacking-statement 'attack-right-conjunction)
			   (let ((right (right-conjunct initial-statement)))
			     (if (and (evenp turn-number)
				      (atomic-formula? right)
				      (not (previously-asserted? dialogue right)))
				 (msg "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
				 (progn
				   (msg "OK, you are responding to the attack at position ~A by asserting ~A~%" index right)
				   (add-move-to-dialogue dialogue (make-defense right index))
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
			     (add-move-to-dialogue dialogue (make-defense assertion index))
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
				   (add-move-to-dialogue dialogue (make-defense consequent index))
				   (close-attack dialogue index)))))
			  ((universal? initial-statement)
			   (let ((instantiated (instantiate attacking-statement (bound-variable initial-statement) (matrix initial-statement))))
			     (msg "OK, you are responding to the attack on your universal statement~%~%  ~A~%~%by asserting ~A"
				     initial-statement instantiated)
			     (add-move-to-dialogue dialogue (make-defense instantiated index))
			     (close-attack dialogue index)))
			  ((existential? initial-statement)
			   (msg "OK, you are defending an existential claim.  To do so, you must provide a witness.~%")
			   (let ((witness nil))
			     (msg "What term do you choose? ")
			     (setf witness (read-term))
			     (let ((instantiated (instantiate witness (bound-variable initial-statement) (matrix initial-statement))))
			       (msg "OK, you've chosen term ~A. Your assertion for this move is thus~%~%  ~A~%" witness instantiated)
			       (add-move-to-dialogue dialogue (make-defense instantiated index))
			       (close-attack dialogue index))))
			  (t (error "Unrecognized defensive move!"))))))))))

(defun start-dialogue (initial-statement)
  (when (atomic-formula? initial-statement)
    (error "A dialogue cannot commence with a composite formula!"))
  (let ((response nil)
	(stance nil)
	(acceptable-input? nil)
	(turn-number 1)
	(dialogue (make-dialogue initial-statement)))
    (msg "Let's play a dialogue for ~A.~%" initial-statement)
    (msg "Turn number 0.~%")
    (msg "Proponent asserts ~A~%" initial-statement)
    (until (eq response 'done)
      (msg "Turn number ~A.~%" turn-number)
      (setq acceptable-input? nil)
      (if (evenp turn-number)
	  (msg "Proponent's turn.~%")
	  (msg "Opponent's turn.~%"))
      (msg "Attack (A), defend (D), print dialogue (P), or quit (Q)? ")
      (setq stance (read-symbol 'a 'd 'q 'p))
      (case stance
	(p (msg "The dialogue so far is:~%~A~%" dialogue))
	(q (setf response 'done))
	(a (extend-dialogue-with-attack dialogue))
	(d (extend-dialogue-with-defense dialogue)))
    (msg "Thanks for playing.~%")
    (msg "The dialogue went like this:~%~A~%" dialogue)
    (msg "Here were the closed attacks: ~A" (dialogue-closed-attacks dialogue)))))


(defun proof-to-strategy (d)
  "Transform the deduction D with endformula x into a strategy for
  winning a dialogue game based on x."
  (declare (ignore d))
  nil)

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