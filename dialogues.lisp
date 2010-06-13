;;; dialogues.lisp Play Lorenzen dialogue games

(in-package :dialogues)

(defun make-move (player statement stance reference)
  (make-instance 'move
		 :player player
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
  (let ((player (move-player move)))
    (string= player "P")))

(defun opponent-move? (move)
  (let ((player (move-player move)))
    (string= player "O")))

(defclass move ()
  ((player :initarg :player
	   :accessor move-player)
   (statement :initarg :statement
	      :accessor move-statement)
   (stance :initarg :stance
	   :accessor move-stance)
   (reference :initarg :reference
	      :accessor move-reference)))

(defmethod print-object ((move move) stream)
  (print-unreadable-object (move stream :type t)
    (with-slots (player statement stance reference) move
      (format stream "player: ~A stance: ~A statement: ~A (in reference to move ~A)"
	      (or player "(unset)")
	      (or stance "(unset)")
	      (or statement "(unset)")
	      (or reference "(unset)")))))

(defun pretty-print-move (move stream)
  (with-slots (stance reference statement)
      move
    (if (and stance reference) ; a non-initial move
	(format stream "[~A,~A] ~A" stance reference statement)
	(format stream "~A (initial move)" statement))))

(defun equal-moves? (move-1 move-2)
  (and (eq (move-player move-1) (move-player move-2))
       (eq (move-stance move-1) (move-stance move-2))
       (let ((ref-1 (move-reference move-1))
	     (ref-2 (move-reference move-2)))
	 (if (integerp ref-1)
	     (and (integerp ref-2) (= ref-1 ref-2))
	     (and (null ref-1) (null ref-2))))
       (equal-statements? (move-statement move-1)
			  (move-statement move-2))))

(defun attacking-move? (move)
  (eq (move-stance move) 'A))

(defun defensive-move? (move)
  (eq (move-stance move) 'D))

(defun initial-move? (move)
  (and (null (move-stance move))
       (null (move-reference move))))

(defun msg-dialogue-so-far (dialogue)
  (msg "The dialogue so far looks like this:")
  (msg "~A" dialogue))

(defun truncate-dialogue (dialogue cutoff)
  (make-instance 'dialogue
		 :signature (dialogue-signature dialogue)
		 :plays (subseq (dialogue-plays dialogue) 0 cutoff)
		 :rules (dialogue-rules dialogue)))

(defvar *prompt* "> ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass symbolic-attack ()
  nil)

(defconstant-if-unbound attack-left-conjunct (make-instance 'symbolic-attack))
(defconstant-if-unbound attack-right-conjunct (make-instance 'symbolic-attack))
(defconstant-if-unbound which-instance? (make-instance 'symbolic-attack))
(defconstant-if-unbound which-disjunct? (make-instance 'symbolic-attack))

(defmethod print-object ((attack (eql attack-left-conjunct)) stream)
  (format stream "ATTACK-LEFT-CONJUNCT"))

(defmethod print-object ((attack (eql attack-right-conjunct)) stream)
  (format stream "ATTACK-RIGHT-CONJUNCT"))

(defmethod print-object ((attack (eql which-instance?)) stream)
  (format stream "WHICH-INSTANCE?"))

(defmethod print-object ((attack (eql which-disjunct?)) stream)
  (format stream "WHICH-DISJUNCT?"))

(defvar symbolic-attacks (list attack-left-conjunct
			       attack-right-conjunct
			       which-instance?
			       which-disjunct?))

(defun symbolic-attack? (obj)
  (eql (class-of obj) 'symbolic-attack))

(defclass statement (formula symbolic-attack term)
  nil)

(defgeneric equal-statements? (statement-1 statement-2))

(defmethod equal-statements? ((form-1 formula) (form-2 formula))
  (equal-formulas? form-1 form-2))

(defmethod equal-statements? ((form formula) (sa symbolic-attack))
  nil)

(defmethod equal-statements? ((form formula) (term term))
  nil)

(defmethod equal-statements? ((sa symbolic-attack) (form formula))
  nil)

(defmethod equal-statements? ((sa-1 symbolic-attack) (sa-2 symbolic-attack))
  (eq sa-1 sa-2))

(defmethod equal-statements? ((sa symbolic-attack) (term term))
  nil)

(defmethod equal-statements? ((term term) (formula formula))
  nil)

(defmethod equal-statements? ((term term) (sa symbolic-attack))
  nil)

(defmethod equal-statements? ((term-1 term) (term-2 term))
  (equal-terms? term-1 term-2))

(defun non-symbolic-attack-term? (obj)
  "Determine whether OBJ is a term different from the symbolic
attacks which, being symbols, do qualify as terms."
  (and (not (symbolic-attack? obj))
       (term? obj)))

(defun non-symbolic-attack-formula? (obj)
  "Determine whether OBJ is a formula different from the symbolic
  attacks which, being simply lisp symbols, do qualify as [atomic]
  formulas)."
  (and (not (symbolic-attack? obj))
       (formula? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(defclass dialogue ()
  ((signature :accessor dialogue-signature
	      :initform nil
	      :initarg :signature)
   (plays :accessor dialogue-plays 
	  :initform nil
	  :initarg :plays)
   (rules :accessor dialogue-rules
	  :initform nil
	  :initarg :rules)))

(defmethod print-object ((game dialogue) stream)
  (print-unreadable-object (game stream :type t)
    (with-slots (signature plays) game
      (format stream "signature: ~A moves: ~A"
	      signature plays))))

(defun make-dialogue (formula signature rules)
  (make-instance 'dialogue
		 :signature signature
		 :plays (list (make-move 'p formula nil nil))
		 :rules rules))

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

(defun pretty-print-dialogue (dialogue stream)
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

(defun dialogue-length (dialogue)
  (length (dialogue-plays dialogue)))

(defun add-move-to-dialogue (dialogue move)
  (setf (dialogue-plays dialogue)
	(append (dialogue-plays dialogue) 
		(list move)))
  dialogue)

(define-condition dialogue-index-out-of-bounds-error (error)
  ((dialogue :initarg :dialogue
	     :reader dialogue)
   (move :initarg :move
	 :reader move)
   (index :initarg :index
	  :reader index))
  (:report (lambda (condition stream)
	     (let ((dialogue (dialogue condition))
		   (move (move condition))
		   (index (index condition)))
	       (format stream "Unable to add the move ~A to the dialogue ~A at sposition ~A: the index is out-of-bounds" move dialogue index)))))

(defun add-move-to-dialogue-at-position (dialogue move position)
  (let ((len (dialogue-length dialogue)))
    (if (<= position len)
	(if (= position len)
	    (add-move-to-dialogue dialogue move)
	    (progn
	      (setf (dialogue-plays dialogue)
		    (append (first-n position (dialogue-plays dialogue))
			    (list move)
			    (nthcdr (1+ position) (dialogue-plays dialogue))))
	      dialogue))
	(error 'dialogue-index-out-of-bounds-error
	       :dialogue dialogue
	       :move move
	       :index position))))

(defun extend-dialogue (dialogue player stance statement reference)
  (add-move-to-dialogue dialogue
			(make-move player statement stance reference))
  dialogue)

(defun copy-dialogue (dialogue)
  (make-instance 'dialogue
		 :signature (dialogue-signature dialogue)
		 :plays (copy-list (dialogue-plays dialogue))))

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
			 (equal-moves? move-1 move-2))
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

(defun next-moves (dialogue player stance)
  (let (result)
    (let* ((subformulas (proper-subformulas (initial-statement dialogue)))
	   (turn-number (dialogue-length dialogue)))
      (dotimes (index turn-number result)
	(dolist (statement (append subformulas symbolic-attacks))
	  (when (every-rule-passes (dialogue-rules dialogue)
				   dialogue
				   player
				   turn-number
				   statement
				   stance
				   index)
	    (push (list statement index)
		  result)))))))

(defun proponent-wins? (dialogue)
  (and (proponent-move? (last-player dialogue))
       (null (next-moves dialogue 'o 'a))
       (null (next-moves dialogue 'o 'd))))

(defun opponent-wins? (dialogue)
  (and (opponent-move? (last-player dialogue))
       (null (next-moves dialogue 'p 'a))
       (null (next-moves dialogue 'p 'd))))

(defun proponent-loses? (dialogue)
  (not (proponent-wins? dialogue)))

(defun opponent-loses? (dialogue)
  (not (opponent-wins? dialogue)))

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
		   (formula? initial-formula))
	      (setf dialogue (make-dialogue initial-formula
					    signature
					    rules))
	      (go initial-move))
	     ((and signature initial-formula)
	      (msg "The given initial formula is not a formula according to~%the given signature.")
	      (yes-or-no-go
	       "Would you like to enter a different signature?"
	       prompt
	       signature
	       initial-move))
	     (signature
	      (go read-initial-formula))
	     (initial-formula
	      (msg "The given signature is empty, but a non-trivial formula was given.")
	      (go signature-then-check-arguments))
	     (t
	      (go signature)))
     signature-then-check-arguments
       (msg "Please supply a signature in which the given formula~%~%  ~A~%~%is actually a formula." initial-formula)
       (setf signature (read-signature prompt))
       (go check-arguments)
     signature
       (msg "Please supply a signature in which the statements of the game will be written.")
       (setf signature (read-signature prompt))
       (go read-initial-formula)
     read-initial-formula
       (msg "Proponent starts by playing a composite formula.")
       (msg "Input a composite formula:")
       (format t "~A" prompt)
       (setf statement nil)
       (until (composite-formula? statement)
	 (restart-case (setf statement (read-composite-formula))
	   (try-another-formula (new-formula) 
	     :report "Enter another formula"
	     :interactive read-new-formula
	     (setf statement new-formula))))
       (setf dialogue (make-dialogue statement signature rules))
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
       (msg "- Q to quit,")
       (msg "- R to rewind to a previous state.")
       (format t "~A" prompt)
       (ecase (read-symbol 'a 'd 'n 'o 'p 'q 'r)
	 (n (go print-next-moves-then-restart))
	 (q (go quit))
	 (o (go print-open-attacks-then-start-move))
	 (p (go print-then-restart))
	 (a (go attack))
	 (d (go defend))
	 (r (go rewind)))
     rewind
       (msg "It is now move #~A.  Rewind to which previous move?" (1- turn-number))
       (msg "Enter:")
       (msg "- a number between 1 and ~A," (1- turn-number))
       (msg "- P to print the dialogue so far and return to this prompt,")
       (msg "- Q to quit,")
       (msg " -R to restart the move.")
       (format t "~A" prompt)
       (let ((response (read-number-in-interval-or-symbol 1 (1- turn-number) 'p 'r)))
	 (when (integerp response)
	   (setf dialogue (truncate-dialogue dialogue response))
	   (setf turn-number response)
	   (go start-move))
	 (ecase response
	   (p (go print-then-rewind))
	   (q (go quit))
	   (r (go start-move))))
     print-then-rewind
       (msg-dialogue-so-far dialogue)
       (go rewind)
     print-next-moves-then-restart
       (let ((next-attacks (next-moves dialogue player 'a))
	     (next-defenses (next-moves dialogue player 'd)))
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
       (let ((next-attacks (next-moves dialogue player 'a)))
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
       (let ((next-defenses (next-moves dialogue player 'a)))
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
       (setf statement nil)
       (until (formula? statement)
	 (restart-case (setf statement (read-formula))
	   (try-another-formula (new-formula) 
	     :report "Enter a different formula."
	     :interactive read-new-formula
	     (setf statement new-formula))))
       (go evaluate-rules)
     term-input
       (msg "Enter a term:")
       (format t "~A" prompt)
       (setf statement (read-term-in-signature signature))
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
	 (l (setf statement attack-left-conjunct))
	 (r (setf statement attack-right-conjunct))
	 (d (setf statement which-disjunct?))
	 (i (setf statement which-instance?)))
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

;;; dialogues.lisp ends hered
