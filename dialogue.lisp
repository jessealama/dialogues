(in-package :dialogues)

(defclass dialogue ()
  ((plays
    :accessor plays
    :type list
    :initform nil
    :initarg :plays)
   (initial-formula
    :accessor initial-formula
    :type formula
    :initarg :initial-formula)
   (ruleset
    :type ruleset
    :accessor ruleset
    :initarg :ruleset
    :initform (error "To make a dialogue, a ruleset is required."))))

(defun empty-dialogue-p (dialogue)
  (null (plays dialogue)))

(defun truncate-dialogue (dialogue cutoff)
  (make-instance 'dialogue
		 :plays (subseq (plays dialogue) 0 cutoff)
                 :initial-formula (initial-formula dialogue)))

;; (defun copy-and-truncate-dialogue (dialogue cutoff)
;;   (truncate-dialogue (copy-dialogue dialogue) cutoff))

(defvar *prompt* "> ")

(defmethod print-object ((game dialogue) stream)
  (print-unreadable-object (game stream :type t)
    (with-slots (plays initial-formula ruleset) game
      (format stream "Ruleset: ~a" ruleset)
      (terpri stream)
      (if (null plays)
          (format stream "1 move")
          (format stream "~d moves" (1+ (length plays))))
      (format stream ":")
      (terpri stream)
      (format stream "0 P ~a [initial move]" initial-formula)
      (unless (null plays)
        (loop
           :initially (format stream "~%")
           :for move :in plays
           :for i :from 1 :upto (length plays)
           :do
           (format stream "~d " i)
           (if (proponent-move-p move)
               (format stream "P ")
               (format stream "O "))
           (format stream "~A [" (statement move))
           (if (attack-p move)
                 (format stream "A")
                 (format stream "D"))
           (format stream ",~d]" (reference move))
           (terpri stream))))))

(defun dialogue-length (dialogue)
  (1+ (length (plays dialogue))))

(defun add-move-to-dialogue (dialogue move)
  (make-instance 'dialogue
                 :plays (append (plays dialogue) (list move))
                 :initial-formula (initial-formula dialogue)
                 :ruleset (ruleset dialogue)))

(defun nth-move (dialogue n)
  (if (zerop n)
      (error "What is the zeroth move?")
      (nth (1- n) (plays dialogue))))

(defun last-move (dialogue)
  (first (last (plays dialogue))))

(defun last-player (dialogue)
  (player (last-move dialogue)))

(defun nth-statement (dialogue n)
  (if (zerop n)
      (initial-formula dialogue)
      (statement (nth-move dialogue n))))

(defun attacking-moves (dialogue)
  (remove-if-not #'attack-p (plays dialogue)))

(defun defensive-moves (dialogue)
  (remove-if-not #'defense-p (plays dialogue)))

(defun moves-referring-to (dialogue reference)
  (remove-if-not #'(lambda (x) (= x reference))
                 (plays dialogue)
                 :key #'reference))

(defun attacks-referring-to (dialogue reference)
  (remove-if-not #'attack-p (moves-referring-to dialogue reference)))

(defun closed-attack-indices (dialogue)
  (mapcar #'reference (defensive-moves dialogue)))

(defun open-attack-indices (dialogue)
  (unless (empty-dialogue-p dialogue)
    (loop
       :with indices = nil
       :with moves = (plays dialogue)
       :for m :in moves
       :for i :from 1
       :do
       (when (attack-p m)
         (unless (some #'(lambda (other-move)
                           (and (defense-p other-move)
                                (= (reference other-move) i)))
                       (subseq moves i))
           (push i indices)))
       :finally
       (return (reverse indices)))))

(defun most-recent-open-attack-on-opponent (dialogue)
  (loop
     :with plays = (plays dialogue)
     :for i :from (dialogue-length dialogue) :downto 1
     :for tail = (subseq plays (1- i))
     :for move = (nth-move dialogue i)
     :when (and (proponent-move-p move)
                (attack-p move)
                (not (some #'(lambda (other-move)
                               (and (opponent-move-p other-move)
                                    (defense-p other-move)
                                    (= (reference other-move) i)))
                           tail))) :do (return i)
     :finally (return nil)))

(defun most-recent-open-attack-on-proponent (dialogue)
  (loop
     :with plays = (plays dialogue)
     :for i :from (dialogue-length dialogue) :downto 1
     :for tail = (subseq plays (1- i))
     :for move = (nth-move dialogue i)
     :when (and (opponent-move-p move)
                (attack-p move)
                (not (some #'(lambda (other-move)
                               (and (proponent-move-p other-move)
                                    (defense-p other-move)
                                    (= (reference other-move) i)))
                           tail))) :do (return i)
     :finally (return nil)))

(defun most-recent-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (first open-attacks)))

(defun earliest-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (first (last open-attacks))))

(defun earliest-open-attack-for-player (dialogue player &key end)
  "The smallest index (starting from 0) of the attacking move by
  PLAYER in DIALOGUE to which there is no response."
  (loop
     with plays = (plays dialogue)
     for i from 1 upto (if end end (length plays))
     for move in (cdr plays)
     do
       (when (attack-p move)
	 (let ((move-player (player move)))
	   (when (eql player move-player)
	     (unless (some #'(lambda (other-move)
			       (and (not (eql (player other-move) player))
				    (defense-p other-move)
				    (= (reference other-move) i)))
			   plays)
	       (return i)))))
     finally
       (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluating rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun evaluate-structural-rule (rule dialogue &key from-end)
;;   (funcall (predicate rule) dialogue :final-move-only from-end))

;; (defun evaluate-particle-rule (rule dialogue)
;;   (let ((precondition (precondition rule))
;; 	(body (body rule)))
;;     (loop
;;        for move in (dialogue-plays dialogue)
;;        for turn-number from 0
;;        do
;; 	 (when (funcall precondition dialogue
;; 			(move-player move)
;; 			turn-number
;; 			(move-statement move)
;; 			(move-stance move)
;; 			(move-reference move))
;; 	     (unless (funcall body dialogue
;; 			      (move-player move)
;; 			      turn-number
;; 			      (move-statement move)
;; 			      (move-stance move)
;; 			      (move-reference move))
;; 	       (return nil)))
;;        finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions of dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun next-moves-at-position (dialogue player stance position)
;;   (let ((result nil)
;; 	(subformulas (proper-subformula-occurrences (initial-formula dialogue)))
;; 	(game-len (dialogue-length dialogue)))
;;     (if (> position game-len)
;; 	nil
;; 	(dotimes (index position result)
;; 	  (dolist (statement (append subformulas
;; 				     *propositional-symbolic-attacks*))
;; 	    (let* ((provisional-move (make-move player statement stance index))
;; 		   (provisional-extension (provisionally-extend-dialogue dialogue provisional-move)))
;; 	      (when (fast-eval-entire-dialogue provisional-extension)
;;                 (let ((pair (list statement index)))
;;                   (pushnew pair result :test #'(lambda (pair-1 pair-2)
;;                                                  (destructuring-bind (statement-1 index-1)
;;                                                      pair-1
;;                                                    (destructuring-bind (statement-2 index-2)
;;                                                        pair-2
;;                                                      (and (= index-1 index-2)
;;                                                           (equal-statements? statement-1 statement-2))))))))))))))

;; (defun all-next-moves-at-position (dialogue position)
;;   (unless (zerop position) ;; not allowed to change the initial move
;;     (let ((diminished-dialogue (copy-and-truncate-dialogue dialogue position)))
;;       (append (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'p statement 'a reference)))
;; 		      (next-moves-at-position diminished-dialogue 'p 'a position))
;; 	      (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'p statement 'd reference)))
;; 		      (next-moves-at-position diminished-dialogue 'p 'd position))
;; 	      (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'o statement 'a reference)))
;; 		      (next-moves-at-position diminished-dialogue 'o 'a position))
;; 	      (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'o statement 'd reference)))
;; 		      (next-moves-at-position diminished-dialogue 'o 'd position))))))

;; (defun all-next-proponent-moves-at-position (dialogue position)
;;   (remove-if-not #'proponent-move?
;; 		 (all-next-moves-at-position dialogue position)))

;; (defun all-next-opponent-moves-at-position (dialogue position)
;;   (remove-if-not #'opponent-move?
;; 		 (all-next-moves-at-position dialogue position)))

(defun continuations (dialogue)
  "A list of moves according to which DIALOGUE could be continued."
  (loop
     :with ruleset = (ruleset dialogue)
     :with l = (funcall (expander ruleset) dialogue)
     :with validator = (validator ruleset)
     :for move :in l
     :for extended-dialogue = (add-move-to-dialogue dialogue move)
     :for check = (funcall validator extended-dialogue)
     :do
     (unless check
       (error "Adding the move~%~%  ~a~%~%to the dialogue~%~%~a~%~%violates the ruleset." move dialogue))
     :finally
     (return (remove-duplicates l :test #'equal-moves?))))

(defun next-attacks (dialogue)
  (remove-if-not #'attack-p (continuations dialogue)))

(defun next-proponent-attacks (dialogue)
  (remove-if-not #'proponent-move-p (next-attacks dialogue)))

(defun next-opponent-attacks (dialogue)
  (remove-if-not #'opponent-move-p (next-attacks dialogue)))

(defun next-defenses (dialogue)
  (remove-if-not #'defense-p (continuations dialogue)))

(defun next-proponent-defenses (dialogue)
  (remove-if-not #'proponent-move-p (next-defenses dialogue)))

(defun next-opponent-defenses (dialogue)
  (remove-if-not #'opponent-move-p (next-defenses dialogue)))

(defun next-proponent-moves (dialogue)
  (remove-if-not #'proponent-move-p (continuations dialogue)))

(defun next-opponent-moves (dialogue)
  (remove-if-not #'opponent-move-p (continuations dialogue)))

(defun proponent-wins-p (dialogue)
  (and (proponent-move-p (last-move dialogue))
       (null (next-opponent-moves dialogue))))

(defun opponent-wins-p (dialogue)
  (and (opponent-move-p (last-move dialogue))
       (null (next-proponent-moves dialogue))))

(defun proponent-loses-p (dialogue)
  (not (proponent-wins-p dialogue)))

(defun opponent-loses-p (dialogue)
  (not (opponent-wins-p dialogue)))

;; (defun freshly-extend-dialogue (dialogue player stance statement reference)
;;   (let ((new-move (make-move player statement stance reference)))
;;     (add-move-to-dialogue (copy-dialogue dialogue) new-move)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Playing games
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun play-dialogue-game (rules initial-formula)
;;   (let ((dialogue nil)
;; 	(turn-number 0)
;; 	(player nil)
;; 	(stance nil)
;; 	(index nil)
;; 	(statement nil)
;; 	(prompt "> "))
;;     (tagbody (go greetings)
;;      greetings
;;        (msg "Let's play a dialogue game!")
;;        (go check-arguments)
;;      check-arguments
;;        (setf dialogue (make-dialogue initial-formula
;;                                      rules))
;;        (go initial-move)
;;      initial-move
;;        (msg "Game on!")
;;        (incf turn-number)
;;        (go start-move)
;;      start-move
;;        (if (evenp turn-number)
;; 	   (setf player 'p)
;; 	   (setf player 'o))
;;        (msg "Turn #~A: ~A" turn-number (ecase player
;; 					   (p "Proponent")
;; 					   (o "Opponent")))
;;        (msg "Enter:")
;;        (msg "- A to attack,")
;;        (msg "- D to defend,")
;;        (msg "- N to see all possible attacks and defenses,")
;;        (msg "- O to list the open attacks at this point,")
;;        (msg "- P to print the dialogue so far,")
;;        (msg "- Q to quit,")
;;        (msg "- R to rewind to a previous state.")
;;        (format t "~A" prompt)
;;        (ecase (read-symbol 'a 'd 'n 'o 'p 'q 'r)
;; 	 (n (go print-next-moves-then-restart))
;; 	 (q (go quit))
;; 	 (o (go print-open-attacks-then-start-move))
;; 	 (p (go print-then-restart))
;; 	 (a (go attack))
;; 	 (d (go defend))
;; 	 (r (go rewind)))
;;      rewind
;;        (msg "It is now move #~A.  Rewind to which previous move?" (1- turn-number))
;;        (msg "Enter:")
;;        (msg "- a number between 1 and ~A," (1- turn-number))
;;        (msg "- P to print the dialogue so far and return to this prompt,")
;;        (msg "- Q to quit,")
;;        (msg " -R to restart the move.")
;;        (format t "~A" prompt)
;;        (let ((response (read-number-in-interval-or-symbol 1 (1- turn-number) 'p 'r)))
;; 	 (when (integerp response)
;; 	   (setf dialogue (truncate-dialogue dialogue response))
;; 	   (setf turn-number response)
;; 	   (go start-move))
;; 	 (ecase response
;; 	   (p (go print-then-rewind))
;; 	   (q (go quit))
;; 	   (r (go start-move))))
;;      print-then-rewind
;;        (msg-dialogue-so-far dialogue)
;;        (go rewind)
;;      print-next-moves-then-restart
;;        (let ((next-attacks (next-moves dialogue player 'a))
;; 	     (next-defenses (next-moves dialogue player 'd)))
;; 	 (cond (next-attacks
;; 		(msg "Possible attacks:")
;; 		(dolist (attack next-attacks)
;; 		  (let ((statement (first attack))
;; 			(reference (second attack)))
;; 		    (msg "Attack move ~A with the statement ~A" reference statement))))
;; 	       (t
;; 		(msg "No attacks are available.")))
;; 	 (cond (next-defenses
;; 		(msg "Possible defenses:")
;; 		(dolist (defense next-defenses)
;; 		  (let ((statement (first defense))
;; 			(reference (second defense)))
;; 		    (msg "Defend against the attack of move ~A with the statement ~A" reference statement))))
;; 	       (t
;; 		(msg "No defenses are available.")))
;; 	 (when (and (null next-attacks)
;; 		    (null next-defenses))
;; 	   (msg "You lose.")))
;;        (go start-move)
;;      print-open-attacks-then-start-move
;;        (let ((open (open-attack-indices dialogue)))
;; 	 (if open
;; 	     (msg "Open attacks at this point: ~A" (comma-separated-list open))
;; 	     (msg "All attacks are closed at this point."))
;; 	 (go start-move))
;;      print-open-attacks-then-defend
;;        (let ((open (open-attack-indices dialogue)))
;; 	 (if open
;; 	     (msg "Open attacks at this point: ~A" (comma-separated-list open))
;; 	     (msg "All attacks are closed at this point."))
;; 	 (go defend))
;;      print-then-restart
;;        (msg-dialogue-so-far dialogue)
;;        (go start-move)
;;      print-then-attack
;;        (msg-dialogue-so-far dialogue)
;;        (go attack)
;;      print-then-defend
;;        (msg-dialogue-so-far dialogue)
;;        (go defend)
;;      print-then-statement
;;        (msg-dialogue-so-far dialogue)
;;        (go statement)
;;      print-then-statement-input
;;        (msg-dialogue-so-far dialogue)
;;        (go statement-input)
;;      print-next-attacks-then-attack
;;        (let ((next-attacks (next-moves dialogue player 'a)))
;; 	 (cond (next-attacks
;; 		(msg "Possible attacks:")
;; 		(dolist (attack next-attacks)
;; 		  (let ((statement (first attack))
;; 			(reference (second attack)))
;; 		    (msg "Attack move ~A with the statement ~A" reference statement)))
;; 		(go attack))
;; 	       (t
;; 		(msg "No attacks are available.")
;; 		(msg "Perhaps you should consider defending...")
;; 		(go start-move))))
;;      attack
;;        (msg "Attack which move? Enter:")
;;        (msg "- An integer between 0 and ~A," (1- turn-number))
;;        (msg "- N to see all possible attacks,")
;;        (msg "- P to print the dialogue so far and come back to this prompt,")
;;        (msg "- Q to quit,")
;;        (msg "- R to restart the move.")
;;        (format t "~A" prompt)
;;        (setf index (read-number-in-interval-or-symbol
;; 		    0 (1- turn-number)
;; 		    'n 'p 'q 'r))
;;        (when (integerp index)
;; 	 (setf stance 'a)
;; 	 (go statement))
;;        (ecase index
;; 	 (n (go print-next-attacks-then-attack))
;; 	 (p (go print-then-attack))
;; 	 (q (go quit))
;; 	 (r (go start-move)))
;;      print-next-defenses-then-defend
;;        (let ((next-defenses (next-moves dialogue player 'a)))
;; 	 (cond (next-defenses
;; 		(msg "Possible defenses:")
;; 		(dolist (defense next-defenses)
;; 		  (let ((statement (first defense))
;; 			(reference (second defense)))
;; 		    (msg "Defend against the attack of move ~A with the statement ~A" reference statement)))
;; 		(go defend))
;; 	       (t
;; 		(msg "No defenses are available.")
;; 		(msg "Perhaps you should consider attacking...")
;; 		(go start-move))))
;;      defend
;;        (msg "Defend against which move? Enter:")
;;        (msg "- An integer between 0 and ~A," (1- turn-number))
;;        (msg "- N to see all possible defenses,")
;;        (msg "- O to list the open attacks at this point,")
;;        (msg "- P to print the dialogue so far and come back to this prompt,")
;;        (msg "- Q to quit,")
;;        (msg "- R to restart the move.")
;;        (format t "~A" prompt)
;;        (setf index (read-number-in-interval-or-symbol
;; 		    0 (1- turn-number)
;; 		    'n 'o 'p 'q 'r))
;;        (when (integerp index)
;; 	 (setf stance 'd)
;; 	 (go statement))
;;        (ecase index
;; 	 (n (go print-next-defenses-then-defend))
;; 	 (o (go print-open-attacks-then-defend))
;; 	 (p (go print-then-defend))
;; 	 (q (go quit))
;; 	 (r (go start-move)))
;;      formula-input
;;        (msg "Enter a formula:")
;;        (format t "~A" prompt)
;;        (setf statement nil)
;;        (until (formula-p statement)
;; 	 (restart-case (setf statement (read-formula))
;; 	   (try-another-formula (new-formula)
;; 	     :report "Enter a different formula."
;; 	     :interactive read-new-formula
;; 	     (setf statement new-formula))))
;;        (go evaluate-rules)
;;      term-input
;;        (msg "Enter a term:")
;;        (format t "~A" prompt)
;;        (setf statement (read-term))
;;        (go evaluate-rules)
;;      statement-input
;;        (if (eq stance 'a)
;; 	   (msg "What is your attack? ")
;; 	   (msg "What is your defense? "))
;;        (msg "Enter:")
;;        (msg "- P to print the dialogue so far and return to this prompt,")
;;        (msg "- Q to quit,")
;;        (msg "- F to type a formula,")
;;        (msg "- T to type a term,")
;;        (when (eq stance 'a)
;; 	 (msg "- L for ATTACK-LEFT-CONJUNCT,")
;; 	 (msg "- R for ATTACK-RIGHT-CONJUNCT,")
;; 	 (msg "- D for WHICH-DISJUNCT?,")
;; 	 (msg "- I for WHICH-INSTANCE?,"))
;;        (format t "~A" prompt)
;;        (ecase (read-symbol 'p 'q 'f 't 'l 'r 'd 'i)
;; 	 (p (go print-then-statement-input))
;; 	 (f (go formula-input))
;; 	 (t (go term-input))
;; 	 (l (setf statement *attack-left-conjunct*))
;; 	 (r (setf statement *attack-right-conjunct*))
;; 	 (d (setf statement *which-disjunct?*))
;; 	 ;; (i (setf statement *which-instance?*))
;;          )
;;        (go evaluate-rules)
;;      statement
;;        (msg "You are responding to move #~A.  Enter:" index)
;;        (msg "- P to print the dialogue so far and come back to this prompt,")
;;        (msg "- Q to quit,")
;;        (msg "- R to restart the move,")
;;        (msg "- S to enter your response to move #~A." index)
;;        (format t "~A" prompt)
;;        (setf statement (read-symbol 'p 'q 'r 's))
;;        (ecase statement
;; 	 (p (go print-then-statement))
;; 	 (q (go quit))
;; 	 (r (go start-move))
;; 	 (s (go statement-input)))
;;      evaluate-rules
;;        (let ((provisional-move (make-move player statement stance index)))
;; 	 (multiple-value-bind (rules-result violated-rules)
;; 	     (eval-entire-dialogue (provisionally-extend-dialogue dialogue provisional-move))
;; 	   (when rules-result
;; 	     (go successful-turn))
;; 	   (msg "At least one of the dialogue rules is violated by your attack:")
;; 	   (dolist (violated-rule violated-rules)
;; 	     (msg "* Rule ~A: ~A" (name violated-rule) (description violated-rule)))
;; 	   (msg "Restarting the move...")
;; 	   (go start-move)))
;;      successful-turn
;;        (incf turn-number)
;;        (add-move-to-dialogue dialogue
;; 			     (make-move player statement stance index))
;;        (go start-move)
;;      quit
;;        (msg "Thanks for playing, I hope you had fun."))
;;     dialogue))

(defun opponent-assertions (dialogue)
  (mapcar #'statement (remove-if-not #'opponent-move-p (plays dialogue))))

(defmethod terms-in ((dialogue dialogue))
  (terms-in (plays dialogue)))

(defmethod free-variables ((dialogue dialogue))
  (free-variables (plays dialogue)))

;;; dialogues.lisp ends hered
