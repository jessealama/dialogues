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
	(format stream "~A (initial move)" statement))))

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

(defun equal-moves? (move-1 move-2 signature)
  (and (eq (move-player move-1) (move-player move-2))
       (eq (move-stance move-1) (move-stance move-2))
       (let ((ref-1 (move-reference move-1))
	     (ref-2 (move-reference move-2)))
	 (if (integerp ref-1)
	     (and (integerp ref-2) (= ref-1 ref-2))
	     (and (null ref-1) (null ref-2))))
       (equal-statements? (move-statement move-1)
			  (move-statement move-2)
			  signature)))

(defun attacking-move? (move)
  (eq (move-stance move) 'A))

(defun defensive-move? (move)
  (eq (move-stance move) 'D))

(defun initial-move? (move)
  (and (null (move-stance move))
       (null (move-reference move))))

(defun print-initial-move (dialogue stream)
  (let ((first-move (first (dialogue-plays dialogue)))
	 (num-moves (dialogue-length dialogue)))
    (if (= num-moves 1)
	(format stream "0 ~A ~A" (move-player first-move)
		                 (move-statement first-move))
	(let* ((num-digits (ceiling (log num-moves 10)))
	       (padding (make-string (+ 5 num-digits) :initial-element #\Space)))
	  (format stream (concatenate 'string "0 ~A " padding "~A~%")
		         (move-player first-move)
		         (move-statement first-move))))))

(defun print-move-at-position (position move stream)
  (let ((statement (move-statement move))
	(stance (move-stance move))
	(ref-index (move-reference move)))
    (if (evenp position)
	(format stream "~A P [~A,~A] ~A" position
		                         stance
					 ref-index
					 statement)
	(format stream "~A O [~A,~A] ~A" position 
	                                 stance
					 ref-index
					 statement))))

(defun print-dialogue (dialogue stream depth)
  (declare (ignore depth))
  (let ((plays (dialogue-plays dialogue)))
    (cond ((null plays) (format stream ""))
	  (t 
	   (print-initial-move dialogue stream)
	   (when (cdr plays)
	     (do ((i 1 (1+ i))
		  (moves (cddr plays) (cdr moves))
		  (move (second plays) (car moves)))
		 ((null moves) (print-move-at-position i move stream))
	       (print-move-at-position i move stream)
	       (format stream "~%")))))))

(defun msg-dialogue-so-far (dialogue)
  (msg "The dialogue so far looks like this:")
  (msg "~A" dialogue))

(defun make-dialogue (initial-statement signature)
  (if (formula? initial-statement signature)
      (let ((first-move (make-proponent-move initial-statement nil nil)))
	(make-dialogue-int :plays (list first-move)
			   :signature signature))
      (error "The given formula~%~%  ~A~%~%is not a formula according to the given signature~%~%  ~A~%" initial-statement signature)))

(defvar *prompt* "> ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar symbolic-attacks
  '(attack-left-conjunct attack-right-conjunct which-instance? which-disjunct?))

(defun symbolic-attack? (obj)
  (member obj symbolic-attacks))

(defun statement? (obj signature)
  (or (formula? signature obj)
      (symbolic-attack? obj)
      (term? signature obj)))

(defun read-statement (signature)
  (let (response)
    (until (statement? response signature)
      (format t "~A" *prompt*)
      (setf response (read t nil nil)))
    response))

(defun read-statement-or-symbols (signature &rest symbols)
  (let (response)
    (until (or (statement? signature response)
	       (member response symbols))
      (format t "~A" *prompt*)
      (setf response (read t nil nil)))
    response))

(defun read-formula-or-term (signature)
  (let (response)
    (until (or (formula? signature response)
	       (term? signature response))
      (format t "~A" *prompt*)
      (setf response (read t nil nil)))
    response))

(defun non-symbolic-attack-term? (obj signature)
  "Determine whether OBJ is a term different from the symbolic
attacks which, being symbols, do qualify as terms."
  (and (not (symbolic-attack? obj))
       (term? obj signature)))

(defun non-symbolic-attack-formula? (obj signature)
  "Determine whether OBJ is a formula different from the symbolic
  attacks which, being simply lisp symbols, do qualify as [atomic]
  formulas)."
  (and (not (symbolic-attack? obj))
       (formula? obj signature)))

(defun equal-statements? (statement-1 statement-2 signature)
  (if (symbolic-attack? statement-1)
      (eq statement-1 statement-2)
      (if (term? statement-1 signature)
	  (equal-terms? statement-1 statement-2 signature)
	  (equal-formulas? statement-1 statement-2 signature))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(defstruct (dialogue
	     (:print-function print-dialogue)
	     (:constructor make-dialogue-int))
  (signature nil :type signature)
  (plays nil :type list))

(defun dialogue-length (dialogue)
  (length (dialogue-plays dialogue)))

(defun add-move-to-dialogue (dialogue move)
  (setf (dialogue-plays dialogue)
	(append (dialogue-plays dialogue) 
		(list move))))

(defun extend-dialogue (dialogue player stance statement reference)
  (add-move-to-dialogue dialogue
			(make-move player statement stance reference))
  dialogue)

(defun freshly-extend-dialogue (dialogue player stance statement reference)
  (let ((copy (copy-dialogue dialogue)))
    (extend-dialogue copy
		     player
		     stance
		     statement
		     reference)
    copy))

(defun equal-dialogues? (dialogue-1 dialogue-2)
  (let ((signature-1 (dialogue-signature dialogue-1))
	(signature-2 (dialogue-signature dialogue-2)))
    (and (equal-signatures? signature-1 signature-2)
	 (equal-length? (dialogue-plays dialogue-1)
			(dialogue-plays dialogue-2))
	 (every-pair #'(lambda (move-1 move-2)
			 (equal-moves? move-1 move-2 signature-1))
		     (dialogue-plays dialogue-1)
		     (dialogue-plays dialogue-2)))))

(defun some-move (predicate dialogue)
  (some predicate (dialogue-plays dialogue)))

(defun every-move (predicate dialogue)
  (every predicate (dialogue-plays dialogue)))

(defun nth-move (dialogue n)
  (nth n (dialogue-plays dialogue)))

(defun initial-statement (dialogue)
  (move-statement (nth-move dialogue 0)))

(defun last-move (dialogue)
  (nth-move dialogue (1- (dialogue-length dialogue))))

(defun last-player (dialogue)
  (move-player (last-move dialogue)))

(defun nth-statement (dialogue n)
  (move-statement (nth-move dialogue n)))

(defun attacking-moves (dialogue)
  (remove-if-not #'attacking-move? (dialogue-plays dialogue)))

(defun defensive-moves (dialogue)
  (remove-if-not #'defensive-move? (dialogue-plays dialogue)))

(defun closed-attack-indices (dialogue)
  (mapcar #'move-reference (defensive-moves dialogue)))

(defun open-attack-indices (dialogue)
  (let ((moves (dialogue-plays dialogue)))
    (when moves
      (do* ((i 0 (1+ i))
	    (move (car moves) (car moves-tail))
	    (moves-tail (cdr moves) (cdr moves-tail))
	    (result (when (attacking-move? move)
		      (list 0))
		    (if (attacking-move? move)
			(cons i result)
			(remove (move-reference move) result))))
	   ((null moves-tail) result)))))

(defun most-recent-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (when open-attacks
      (car open-attacks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concrete dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter peirce-dialogue
  (make-dialogue peirce-formula pqrs-propositional-signature))

(defparameter excluded-middle-dialogue
  (make-dialogue excluded-middle pqrs-propositional-signature))

(defparameter markov-dialogue
  (make-dialogue markov-formula pqrs-propositional-signature))

(defparameter double-negation-dialogue
  (make-dialogue double-negation-intro pqrs-propositional-signature))

(defparameter k-dialogue
  (make-dialogue k-formula pqrs-propositional-signature))

(defparameter b-dialogue
  (make-dialogue b-formula pqrs-propositional-signature))

(defparameter c-dialogue
  (make-dialogue c-formula pqrs-propositional-signature))

(defparameter w-dialogue
  (make-dialogue w-formula pqrs-propositional-signature))

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
	   (warn "An error occurred while evaluating the condition for rule ~A!~%The type of the error was ~A.~%The dialogue at this point is:~%~A~%player: ~A~%position: ~A~%statement: ~A~%stance: ~A~%reference: ~A~%Continuing..." 
		 (quote ,name)
		 ,condition-error
		 dialogue
		 current-player
		 current-position
		 current-statement
		 current-stance
		 current-reference)
	   (if ,condition-result
	       (with-value-and-error (,body-result ,body-error)
		   ,body
		 (if ,body-error 
		     (warn "An error occurred while evaluating the body of rule ~A!~%The type of the error was ~A.~%The dialogue at this point is:~%~A~%player: ~A~%position: ~A~%statement: ~A~%stance: ~A~%reference: ~A~%Continuing..." 
			   (quote ,name)
			   ,body-error
			   dialogue
			   current-player
			   current-position
			   current-statement
			   current-stance
			   current-reference)
		     (values ,body-result (format nil (concatenate 'string "[~A] " ,failure-message) (quote ,name)))))
	       (values t nil)))))))

(defun attack? (stance)
  (eq stance 'a))

(defun defense? (stance)
  (eq stance 'd))

(defmacro make-defensive-rule (&key name
			            (condition t)
			            body
				    failure-message)
  `(make-rule :name ,name
	      :condition (and (defense? current-stance)
			      ,condition)
	      :body ,body
	      :failure-message ,failure-message))

(defmacro make-offensive-rule (&key name
			            (condition t)
			            body
			            failure-message)
  `(make-rule :name ,name
	      :condition (and (attack? current-stance)
			      ,condition)
	      :body ,body
	      :failure-message ,failure-message))

(defmacro with-original-statement ((original-statement) &body body)
  (let ((attack (gensym))
	(attack-refers-to (gensym))
	(original-move (gensym)))
    `(let ((,attack (nth-move dialogue current-reference)))
       (when ,attack
	 (let ((,attack-refers-to (move-reference ,attack)))
	   (when ,attack-refers-to
	     (let ((,original-move (nth-move dialogue ,attack-refers-to)))
	       (when ,original-move
		 (let ((,original-statement (move-statement ,original-move)))
		   (when ,original-statement
		     ,@body))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluating rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-all-rules (rules dialogue player turn-number statement stance index &optional messages)
  (if (null rules)
      (if messages
	  (values nil messages)
	  (values t nil))
      (let ((rule (car rules)))
	(multiple-value-bind (result error-message)
	    (funcall rule dialogue player turn-number statement stance index)
	  (evaluate-all-rules (cdr rules)
			      dialogue
			      player
			      turn-number 
			      statement 
			      stance 
			      index
			      (if result
				  messages
				  (cons error-message messages)))))))

(defun every-rule-passes (rules dialogue player turn-number statement stance index)
  (or (null rules)
      (let ((rule (car rules)))
	(let ((result (funcall rule dialogue 
			            player
				    turn-number
				    statement
				    stance
				    index)))
	  (when result
	    (every-rule-passes (cdr rules)
			       dialogue
			       player
			       turn-number
			       statement
			       stance
			       index))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions of dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-moves (dialogue rules player stance)
  (let (result)
    (let* ((subformulas (proper-subformulas (initial-statement dialogue)))
	   (turn-number (dialogue-length dialogue)))
      (dotimes (index turn-number result)
	(dolist (statement (append subformulas symbolic-attacks))
	  (when (every-rule-passes rules
				   dialogue
				   player
				   turn-number
				   statement
				   stance
				   index)
	    (push (list statement index)
		  result)))))))

(defun proponent-wins? (dialogue rules)
  (and (eq (last-player dialogue) 'p)
       (null (next-moves dialogue rules 'o 'a))
       (null (next-moves dialogue rules 'o 'd))))

(defun opponent-wins? (dialogue rules)
  (and (eq (last-player dialogue) 'o)
       (null (next-moves dialogue rules 'p 'a))
       (null (next-moves dialogue rules 'p 'd))))

(defun proponent-loses? (dialogue rules)
  (not (proponent-wins? dialogue rules)))

(defun opponent-loses? (dialogue rules)
  (not (opponent-wins? dialogue rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Playing games
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play-dialogue-game (rules &optional signature initial-formula)
  (let ((dialogue nil)
	(turn-number 0)
	(player nil)
	(stance nil)
	(index nil)
	(statement nil)
	(prompt "> "))
    (tagbody (go greetings)
     greetings
       (msg "Let's play a dialogue game!")
       (go check-arguments)
     check-arguments
       (cond ((and signature
		   initial-formula 
		   (formula? initial-formula signature))
	      (setf dialogue (make-dialogue initial-formula
					    signature))
	      (go initial-move))
	     ((and signature initial-formula)
	      (msg "The given initial formula is not a formula according to~%the given signature.")
	      (yes-or-no-go
	       "Would you like to enter a different signature?"
	       prompt
	       signature
	       initial-move))
	     (signature
	      (go initial-move))
	     (initial-formula
	      (msg "The given signature is empty, but a non-trivial formula was given.")
	      (go signature-then-check-arguments)))
     signature-then-check-arguments
       (msg "Please supply a signature in which the given formula~%~%  ~A~%~%is actually a formula." initial-formula)
       (setf signature (read-signature prompt))
       (go check-arguments)
     signature
       (msg "Please supply a signature in which the sentences will be written.")
       (setf signature (read-signature prompt))
       (go read-initial-formula)
     read-initial-formula
       (msg "Proponent starts by playing a composite formula.")
       (msg "Input a composite formula:")
       (format t "~A" prompt)
       (setf dialogue (make-dialogue (read-composite-formula signature)
				     signature))
     initial-move
       (msg "Game on!")
       (incf turn-number)
       (go start-move)
     start-move
       (if (evenp turn-number)
	   (setf player 'p)
	   (setf player 'o))
       (msg "Turn #~A: ~A" turn-number (ecase player
					   (p "Proponent")
					   (o "Opponent")))
       (msg "Enter:")
       (msg "- A to attack,")
       (msg "- D to defend,")
       (msg "- N to see all possible attacks and defenses,")
       (msg "- O to list the open attacks at this point,")
       (msg "- P to print the dialogue so far,")
       (msg "- Q to quit.")
       (format t "~A" prompt)
       (ecase (read-symbol 'a 'd 'n 'o 'p 'q)
	 (n (go print-next-moves-then-restart))
	 (q (go quit))
	 (o (go print-open-attacks-then-start-move))
	 (p (go print-then-restart))
	 (a (go attack))
	 (d (go defend)))
     print-next-moves-then-restart
       (let ((next-attacks (next-moves dialogue rules player 'a))
	     (next-defenses (next-moves dialogue rules player 'd)))
	 (cond (next-attacks
		(msg "Possible attacks:")
		(dolist (attack next-attacks)
		  (let ((statement (first attack))
			(reference (second attack)))
		    (msg "Attack move ~A with the statement ~A" reference statement))))
	       (t
		(msg "No attacks are available.")))
	 (cond (next-defenses
		(msg "Possible defenses:")
		(dolist (defense next-defenses)
		  (let ((statement (first defense))
			(reference (second defense)))
		    (msg "Defend against the attack of move ~A with the statement ~A" reference statement))))
	       (t
		(msg "No defenses are available.")))
	 (when (and (null next-attacks)
		    (null next-defenses))
	   (msg "You lose.")))
       (go start-move)
     print-open-attacks-then-start-move
       (let ((open (open-attack-indices dialogue)))
	 (if open
	     (msg "Open attacks at this point: ~A" (comma-separated-list open))
	     (msg "All attacks are closed at this point."))
	 (go start-move))
     print-open-attacks-then-defend
       (let ((open (open-attack-indices dialogue)))
	 (if open
	     (msg "Open attacks at this point: ~A" (comma-separated-list open))
	     (msg "All attacks are closed at this point."))
	 (go defend))       
     print-then-restart
       (msg-dialogue-so-far dialogue)
       (go start-move)
     print-then-attack
       (msg-dialogue-so-far dialogue)
       (go attack)
     print-then-defend
       (msg-dialogue-so-far dialogue)
       (go defend)
     print-then-statement
       (msg-dialogue-so-far dialogue)
       (go statement)
     print-then-statement-input
       (msg-dialogue-so-far dialogue)
       (go statement-input)
     print-next-attacks-then-attack
       (let ((next-attacks (next-moves dialogue rules player 'a)))
	 (cond (next-attacks
		(msg "Possible attacks:")
		(dolist (attack next-attacks)
		  (let ((statement (first attack))
			(reference (second attack)))
		    (msg "Attack move ~A with the statement ~A" reference statement)))
		(go attack))
	       (t
		(msg "No attacks are available.")
		(msg "Perhaps you should consider defending...")
		(go start-move))))
     attack
       (msg "Attack which move? Enter:")
       (msg "- An integer between 0 and ~A," (1- turn-number))
       (msg "- N to see all possible attacks,")
       (msg "- P to print the dialogue so far and come back to this prompt,")
       (msg "- Q to quit,")
       (msg "- R to restart the move.")
       (format t "~A" prompt)
       (setf index (read-number-in-interval-or-symbol 
		    0 (1- turn-number) 
		    'n 'p 'q 'r))
       (when (integerp index)
	 (setf stance 'a)
	 (go statement))
       (ecase index
	 (n (go print-next-attacks-then-attack))
	 (p (go print-then-attack))
	 (q (go quit))
	 (r (go start-move)))
     print-next-defenses-then-defend
       (let ((next-defenses (next-moves dialogue rules player 'a)))
	 (cond (next-defenses
		(msg "Possible defenses:")
		(dolist (defense next-defenses)
		  (let ((statement (first defense))
			(reference (second defense)))
		    (msg "Defend against the attack of move ~A with the statement ~A" reference statement)))
		(go defend))
	       (t
		(msg "No defenses are available.")
		(msg "Perhaps you should consider attacking...")
		(go start-move))))
     defend
       (msg "Defend against which move? Enter:")
       (msg "- An integer between 0 and ~A," (1- turn-number))
       (msg "- N to see all possible defenses,")
       (msg "- O to list the open attacks at this point,")
       (msg "- P to print the dialogue so far and come back to this prompt,")
       (msg "- Q to quit,")
       (msg "- R to restart the move.")
       (format t "~A" prompt)
       (setf index (read-number-in-interval-or-symbol 
		    0 (1- turn-number) 
		    'n 'o 'p 'q 'r))
       (when (integerp index)
	 (setf stance 'd)
	 (go statement))
       (ecase index
	 (n (go print-next-defenses-then-defend))
	 (o (go print-open-attacks-then-defend))
	 (p (go print-then-defend))
	 (q (go quit))
	 (r (go start-move)))
     formula-input
       (msg "Enter a formula:")
       (format t "~A" prompt)
       (setf statement (read-formula (dialogue-signature dialogue)))
       (go evaluate-rules)
     term-input
       (msg "Enter a term:")
       (format t "~A" prompt)
       (setf statement (read-term))
       (go evaluate-rules)
     statement-input
       (if (eq stance 'a)
	   (msg "What is your attack? ")
	   (msg "What is your defense? "))
       (msg "Enter:")
       (msg "- P to print the dialogue so far and return to this prompt,")
       (msg "- Q to quit,")
       (msg "- F to type a formula,")
       (msg "- T to type a term,")
       (when (eq stance 'a)
	 (msg "- L for ATTACK-LEFT-CONJUNCT,")
	 (msg "- R for ATTACK-RIGHT-CONJUNCT,")
	 (msg "- D for WHICH-DISJUNCT?,")
	 (msg "- I for WHICH-INSTANCE?,"))
       (format t "~A" prompt)
       (ecase (read-symbol 'p 'q 'f 't 'l 'r 'd 'i)
	 (p (go print-then-statement-input))
	 (f (go formula-input))
	 (t (go term-input))
	 (l (setf statement 'attack-left-conjunct))
	 (r (setf statement 'attack-right-conjunct))
	 (d (setf statement 'which-disjunct?))
	 (i (setf statement 'which-instance?)))
       (go evaluate-rules)
     statement
       (msg "You are responding to move #~A.  Enter:" index)
       (msg "- P to print the dialogue so far and come back to this prompt,")
       (msg "- Q to quit,")
       (msg "- R to restart the move,")
       (msg "- S to enter your response to move #~A." index)
       (format t "~A" prompt)
       (setf statement (read-symbol 'p 'q 'r 's))
       (ecase statement
	 (p (go print-then-statement))
	 (q (go quit))
	 (r (go start-move))
	 (s (go statement-input)))
     evaluate-rules
       (multiple-value-bind (rules-result messages)
	   (evaluate-all-rules rules dialogue player turn-number statement stance index)
	 (when rules-result
	   (go successful-turn))
	 (msg "At least one of the dialogue rules is violated by your attack:")
	 (dolist (message messages)
	   (msg "* ~A" message))
	 (msg "Restarting the move...")
	 (go start-move))
     successful-turn
       (incf turn-number)
       (add-move-to-dialogue dialogue
			     (make-move player statement stance index))
       (go start-move)
     quit
       (msg "Thanks for playing, I hope you had fun."))
    dialogue))

(provide 'dialogues)

;;; dialogues.lisp ends here