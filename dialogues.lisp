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
  (let ((first-move (make-move :statement initial-statement
			       :stance nil
			       :reference nil)))
    (make-dialogue-int :plays (list first-move)
		       :length 1
		       :closed-attacks nil)))


(defstruct (dialogue
	     (:print-function print-dialogue)
	     (:constructor make-dialogue-int))
  (plays nil :type list)
  (length 0 :type number)
  (closed-attacks nil :type list))

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

(defun start-dialogue (initial-statement)
  (when (atomic-formula? initial-statement)
    (error "A dialogue cannot commence with a composite formula!"))
  (let ((response nil)
	(stance nil)
	(index nil)
	(acceptable-input? nil)
	(turn-number 1)
	(dialogue (make-dialogue initial-statement)))
    (format t "Let's play a dialogue for ~A.~%" initial-statement)
    (format t "Turn number 0.~%")
    (format t "Proponent asserts ~A~%" initial-statement)
    (until (eq response 'done)
      (format t "Turn number ~A.~%" turn-number)
      (format t "Type DONE to end the game now.~%")
      (setq acceptable-input? nil)
      (if (evenp turn-number)
	  (format t "Proponent's turn.~%")
	  (format t "Opponent's turn.~%"))
      (until acceptable-input?
        (format t "Attack (A) or defend (D)? ")
	(setq stance (read-symbol 'a 'd))
	(if (eq stance 'a)
	    (format t "Attack which statement? (Your response should be a number between 0 and ~A.) " (1- turn-number))
	    (format t "Defend against which attack? (Your response should be a number between 0 and ~A. " (1- turn-number)))
	(setq index (read-non-negative-number-at-most turn-number))
	(cond ((= (mod turn-number 2) (mod index 2))
	       (format t "One cannot attack oneself or defend against one's own attacks!~%"))
	      ((eq stance 'a)
	       (if (member index (dialogue-closed-attacks dialogue))
		   (format t "One cannot attack move ~A because that attack has already been responded to.~%" index)
		   (let ((attacked-move (nth index (dialogue-plays dialogue))))
		     (let ((attacked-statement (move-statement attacked-move)))
		       (if (atomic-formula? attacked-statement)
			   (format t "One cannot attack atomic statements!~%")
			   (cond ((conjunction? attacked-statement)
				  (format t "The attacked statement,~%~%  ~A,~%~%is a conjunction. Attack the left or right conjunct? " attacked-statement)
				  (let ((left-or-right (read-symbol 'l 'r)))
				    (format t "OK, you are attacking move ~A by challenging the other player to defend ~A.~%" index
					                                                         (if (eq left-or-right 'l)
												     (left-conjunct attacked-statement)
												     (right-conjunct attacked-statement)))
				    (add-move-to-dialogue dialogue
							  (if (eq left-or-right 'l)
							      (make-move 'attack-left-conjunction 'a index)
							      (make-move 'attack-right-conjunction 'a index)))))
				 ((disjunction? attacked-statement)
				  (format t "OK, you are attacking move ~A by challenging the other player to specify one of the disjuncts and defend it.~%" index)
				  (add-move-to-dialogue dialogue (make-move 'which-disjunct? 'a index)))
				 ((implication? attacked-statement)
				  (let ((antecedent (antecedent attacked-statement))
					(consequent (consequent attacked-statement)))
				    (if (and (zerop (mod turn-number 2))
					     (atomic-formula? antecedent)
					     (not (member antecedent (dialogue-plays dialogue) :key #'move-statement
							                                       :test #'equalp)))
					(format t "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
					(progn
					  (format t "OK, you are attacking move ~A by asserting ~A and asking the other player to defend ~A~%" index antecedent consequent)
					  (add-move-to-dialogue dialogue (make-move antecedent 'a index))))))
				 ((negation? attacked-statement)
				  (let ((unnegated (unnegate attacked-statement)))
				    (if (and (zerop (mod turn-number 2))
					     (atomic-formula? unnegated)
					     (not (member unnegated (dialogue-plays dialogue) :key #'move-statement
							                                      :test #'equalp)))
					(format t "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
					(progn
					  (format t "OK, you are attacking move ~A by asserting ~A.~%" index unnegated)
					  (add-move-to-dialogue dialogue (make-move unnegated 'a index))))))
				 ((universal? attacked-statement)
				  (let ((var (bound-variable attacked-statement))
					(formula (matrix attacked-statement))
					(instance nil))
				    (format t "OK, you are attacking move ~A, a universal statement.~%" attacked-statement)
				    (format t "To attack a universal, you provide a term t; the other player must then defend the instantiated statement.~%.")
				    (format t "What term do you choose? (Your answer must be a symbol.) ")
				    (setq instance (read-term))
				    (format t "OK, the other player must now defend the statement ~A~%" (instantiate instance var formula))
				    (add-move-to-dialogue dialogue (make-move instance 'a index))))
				 (t ;; existential case
				  (format t "OK, you are attacking move ~A, an existential statement.~%" attacked-statement)
				  (add-move-to-dialogue dialogue (make-move 'which-instance? 'a index)))))))))
	      ((eq stance 'd)
	       (let ((attacking-move (nth index (dialogue-plays dialogue))))
		 (let ((attacking-statement (move-statement attacking-move))
		       (attack-refers-to (move-reference attacking-move)))
		   (if (member attack-refers-to (dialogue-closed-attacks dialogue))
		       (format t "The attack against which you are defending has already been responded to.~%")
		       (let ((initial-move (nth attack-refers-to (dialogue-plays dialogue))))
			 (let ((initial-statement (move-statement initial-move)))
			   (cond ((eq attacking-statement 'attack-left-conjunction)
				  (let ((left (left-conjunct initial-statement)))
				    (if (and (zerop (mod turn-number 2))
					     (atomic-formula? left)
					     (not (member left (dialogue-plays dialogue) :key #'move-statement
							                                 :test #'equalp)))
					(format t "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
					(progn
					  (format t "OK, you are responding to the attack at position ~A by asserting ~A~%" index left)
					  (add-move-to-dialogue dialogue (make-move left 'd index))))))
				 ((eq attacking-statement 'attack-right-conjunction)
				  (let ((right (right-conjunct initial-statement)))
				    (if (and (zerop (mod turn-number 2))
					     (atomic-formula? right)
					     (not (member right (dialogue-plays dialogue) :key #'move-statement
							                                  :test #'equalp)))
					(format t "Proponent cannot assert an atomic formula that has not yet been asserted by Opponent.~%")
					(progn
					  (format t "OK, you are responding to the attack at position ~A by asserting ~A~%" index right)
					  (add-move-to-dialogue dialogue (make-move right 'd index))))))
				 ((eq attacking-statement 'which-disjunct?)
				  (let ((left-or-right nil)
					(assertion nil))
				    (format t "You are responding to an attack on a disjunction:~%~%  ~A~%~%You must now choose either the left or the right disjunct.~%" initial-statement)
				    (setq left-or-right (read-symbol 'l 'r))
				    (setq assertion (if (eq left-or-right 'l)
							(left-disjunct initial-statement)
							(right-disjunct initial-statement)))
				    (format t "OK, you are responding to the attack by asserting ~A~%" assertion)
				    (add-move-to-dialogue dialogue (make-move assertion 'd index))))
				 ((negation? initial-statement)
				  (format t "You cannot (directly) defend against an attack on a negation.~%"))
				 ((implication? initial-statement)
				  (let ((consequent (consequent initial-statement)))
				    (format t "OK, you are responding to the attack on the implication~%~%  ~A~%~%by asserting ~A.~%" initial-statement consequent)
				    (add-move-to-dialogue dialogue (make-move consequent 'd index))))
				 ((universal? initial-statement)
				  (let ((instantiated (instantiate attacking-statement (bound-variable initial-statement) (matrix initial-statement))))
				    (format t "OK, you are responding to the attack on your universal statement~%~%  ~A~%~%by asserting ~A"
					      initial-statement instantiated)
				    (add-move-to-dialogue dialogue (make-move instantiated 'd index))))
				 ((existential? initial-statement)
				  (format t "OK, you are defending an existential claim.  To do so, you must provide a witness.~%")
				  (let ((witness nil))
				    (format t "What term do you choose? ")
				    (setf witness (read-term))
				    (let ((instantiated (instantiate witness (bound-variable initial-statement) (matrix initial-statement))))
				      (format t "OK, you've chosen term ~A. Your assertion for this move is thus~%~%  ~A~%" witness instantiated)
				      (add-move-to-dialogue dialogue (make-move instantiated 'd index)))))
				 (t (error "Unrecognized defensive move!")))))))))
	      (t (setq acceptable-input? t)
	         (incf turn-number)))))
      (format t "Thanks for playing.~%")
      dialogue))

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