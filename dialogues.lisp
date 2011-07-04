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

(defun make-proponent-attack (statement reference)
  (make-attack 'p statement reference))

(defun make-opponent-attack (statement reference)
  (make-attack 'o statement reference))

(defun make-proponent-defense (statement reference)
  (make-defense 'p statement reference))

(defun make-opponent-defense (statement reference)
  (make-defense 'o statement reference))

(defun proponent-move? (move)
  (let ((player (move-player move)))
    (string= player "P")))

(defun opponent-move? (move)
  (let ((player (move-player move)))
    (string= player "O")))

(defun other-player (player)
  (case player
      (p 'o)
      (o 'p)))

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
  (eq (move-stance move) 'a))

(defun defensive-move? (move)
  (eq (move-stance move) 'd))

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

(defun copy-and-truncate-dialogue (dialogue cutoff)
  (truncate-dialogue (copy-dialogue dialogue) cutoff))

(defvar *prompt* "> ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; Dialogue rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dialogue-rule ()
  ((name :initarg :name
	 :accessor name
	 :initform "(no name was supplied)"
	 :type string)
   (description :initarg :description
		:accessor description
		:initform "(no description was supplied)"
		:type string)))

(defmethod print-object ((rule dialogue-rule) stream)
  (print-unreadable-object (rule stream :type t)
    (format stream "~A" (name rule))))

(defmacro make-structural-rule (&key name description predicate)
  (let ((pred-as-fun `(lambda (dialogue &key final-move-only)
			(declare (ignorable dialogue final-move-only))
			,predicate)))
    `(make-instance 'structural-rule
		    :name ,name
		    :description ,description
		    :predicate ,pred-as-fun)))

(defclass particle-rule (dialogue-rule)
  ((precondition :initarg :precondition
		 :accessor precondition
		 :type function
		 :documentation "A function of six arguments (DIALOGUE CURRENT-PLAYER CURRENT-POSITION CURRENT-STATEMENT CURRENT-STANCE CURRENT-REFERENCE).  It if evaluates to NIL when given these inputs, which are understood as a dialogue together with a proposed move to place at the end of the dialogue, it is interpreted as failing to satisfy the condition of the rule.")
   (body :initarg :body
	 :accessor body
	 :type function
	 :documentation "A function of six arguments (DIALOGUE CURRENT-PLAYER CURRENT-POSITION CURRENT-STATEMENT CURRENT-STANCE CURRENT-REFERENCE).  It if evaluates to NIL when given these inputs, which are understood as a dialogue together with a proposed move to place at the end of the dialogue, it is interpreted as satisfying the \"body\" of the rule in question.  The function can assume that the predicate defined in the PRECONDITION slot holds (so that, e.g., certain objects that are tested for existence by the condition can be assumed to exist by this function).")))

(defun particle-rule? (thing)
  (typep thing 'particle-rule))

(defmacro make-particle-rule (&key name description precondition body)
  `(make-instance 'particle-rule
		  :name ,name
		  :description ,description
		  :precondition (lambda (dialogue 
					 current-player
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
				  ,precondition)
		  :body (lambda (dialogue 
				 current-player
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
			  ,body)))

(defclass structural-rule (dialogue-rule)
  ((predicate :initarg :predicate
	      :type function
	      :accessor predicate)))	      

(defun structural-rule? (thing)
  (typep thing 'structural-rule))

(defun attack? (stance)
  (eq stance 'a))

(defun defense? (stance)
  (eq stance 'd))

(defmacro make-defensive-rule (&key name
			            (precondition t)
			            body
				    description)
  `(make-particle-rule
    :name ,name
    :precondition (and (defense? current-stance)
		       ,precondition)
    :body ,body
    :description ,description))

(defmacro make-offensive-rule (&key name
			            (precondition t)
			            body
			            description)
  `(make-particle-rule
    :name ,name
    :precondition (and (attack? current-stance)
		       ,precondition)
    :body ,body
    :description ,description))

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
;;; Rulesets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ruleset ()
  ((rules :initarg :rules
	  :accessor rules
	  :initform nil
	  :type list)
   (name
    :initarg :name
    :accessor name
    :initform "(no name was supplied)"
    :type string)
   (description :initarg :description
		:accessor description
		:initform "(no description was supplied)"
		:type string)))

(defmethod print-object ((ruleset ruleset) stream)
  (print-unreadable-object (ruleset stream :type t)
    (with-slots (name description rules)
	ruleset
      (format stream "~a: ~a" name description))))

(defun equal-rulesets? (ruleset-1 ruleset-2)
  (eq ruleset-1 ruleset-2)) ;; I don't have an interesting notion of equality

(defun copy-ruleset (ruleset)
  (make-instance 'ruleset
		 :rules (copy-list (rules ruleset))
		 :name (name ruleset)
		 :description (description ruleset)))

(defun add-rule-to-ruleset (rule ruleset)
  (pushnew rule (rules ruleset))
  ruleset)

(defun fast-eval-entire-dialogue (dialogue &key structural-rules-from-end)
  "Evaluate all rules, but return only whether every rule passes.
This function is used in cases where it doesn't matter what rules
fail, only whether all of them are satisfied."
  (loop 
     with ruleset = (dialogue-rules dialogue)
     for rule in (rules ruleset)
     do
       (let (rule-passes)
	 (if (structural-rule? rule)
	     (setf rule-passes (evaluate-structural-rule rule dialogue :from-end structural-rules-from-end))
	     (setf rule-passes (evaluate-particle-rule rule dialogue)))
	 (unless rule-passes
	   (return nil)))
     finally (return t)))

(defun eval-entire-dialogue (dialogue &key structural-rules-from-end)
  (loop 
     with failures = nil
     with all-pass = t
     with ruleset = (dialogue-rules dialogue)
     for rule in (rules ruleset)
     do
       (let (rule-passes)
	 (if (structural-rule? rule)
	     (setf rule-passes (evaluate-structural-rule rule dialogue :from-end structural-rules-from-end))
	     (setf rule-passes (evaluate-particle-rule rule dialogue)))
	 (unless rule-passes
	   (setf all-pass nil)
	   (push rule failures)))
     finally (return (values all-pass failures))))

(defun eval-provisional-dialogue (dialogue player statement stance reference)
  (let* ((provisional-move (make-move player statement stance reference))
	 (provisional-dialogue (provisionally-extend-dialogue dialogue provisional-move)))
    (eval-entire-dialogue provisional-dialogue)))

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
	  :initform (make-instance 'ruleset)
	  :initarg :rules
	  :type ruleset)))

(defmethod print-object ((game dialogue) stream)
  (print-unreadable-object (game stream :type t)
    (with-slots (rules signature plays) game
      (format stream "rules: ~A~%" rules)
      ;; (format stream "signature: ~A~%" signature)
      (format stream "moves: ")
      (if (null plays)
	  (format stream "(none)~%")
	  (loop
	     initially (format stream "~%")
	     for i from 0
	     for move in plays
	     do
	       (with-slots (player statement stance reference)
		   move
		   (if (and (null stance)
			    (null reference))
		       (format stream "~d ~A ~A (initial move)~%" i player statement)
		       (format stream "~d ~A ~A [~A,~A]~%" i player statement stance reference))))))))

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

(define-condition inappropriate-initial-statement-error (error)
  ((statement :initarg :formula
	      :reader formula)
   (rules :initarg :rules
	  :type ruleset
	  :reader rules))
  (:report (lambda (condition stream)
	     (let ((statement (statement condition))
		   (rules (rules condition)))
	       (format stream
		       "Dialogues adhereing to the rules~%~%  ~A~%~%cannot begin with the statement~%~%  ~A"
		       (description rules)
		       statement)))))

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

(defun add-attack-to-dialogue-at-position (dialogue player statement ref pos)
  (add-move-to-dialogue-at-position dialogue
				    (make-attack player statement ref)
				    pos))

(defun add-defense-to-dialogue-at-position (dialogue player statement ref pos)
  (add-move-to-dialogue-at-position dialogue
				    (make-defense player statement ref)
				    pos))

(defun provisionally-extend-dialogue (dialogue move)
  "Make a new dialogue from DIALOGUE that has MOVE at the end."
  (make-instance 'dialogue
		 :plays (append (dialogue-plays dialogue)
				(list move))
		 :rules (dialogue-rules dialogue)
		 :signature (dialogue-signature dialogue)))

(defun copy-dialogue (dialogue)
  (make-instance 'dialogue
		 :signature (dialogue-signature dialogue)
		 :plays (copy-list (dialogue-plays dialogue))
		 :rules (dialogue-rules dialogue)))						   

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

(defun some-move (predicate dialogue &key end)
  (some predicate (subseq (dialogue-plays dialogue) 0 end)))

(defun every-move (predicate dialogue &key end)
  (every predicate (subseq (dialogue-plays dialogue) 0 end)))

(defun every-defensive-move (predicate dialogue &key end)
  (every predicate (remove-if-not #'defensive-move?
				  (subseq (dialogue-plays dialogue) 0 end))))

(defun every-proponent-move (predicate dialogue &key end)
  (every predicate (remove-if-not #'proponent-move?
				  (subseq (dialogue-plays dialogue) 0 end))))

(defun every-opponent-move (predicate dialogue &key end)
  (every predicate (remove-if-not #'opponent-move?
				  (subseq (dialogue-plays dialogue) 0 end))))

;; (defun select-moves (predicate dialogue &key start end)
;;   (remove-if-not predicate (dialogue-plays dialogue) 
;; 		 :start (if (null start)
;; 			    0
;; 			    start)
;; 		 :end end))

(defun select-moves (predicate dialogue &key end start)
  (loop
     with winners = nil
     for elt in (subseq (dialogue-plays dialogue)
			(if (null start)
			    0
			    start)
			end)
     do
       (when (funcall predicate elt)
	 (push elt winners))
     finally (return winners)))

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

(defun moves-referring-to (dialogue reference &key end)
  (select-moves #'(lambda (move)
		    (unless (initial-move? move)
		      (= (move-reference move) reference)))
		dialogue
		:end end))

(defun attacks-referring-to (dialogue reference &key end)
  (select-moves #'(lambda (move)
		    (unless (initial-move? move)
		      (when (attacking-move? move)
			(= (move-reference move) reference))))
		dialogue
		:end end))

(defun closed-attack-indices (dialogue)
  (mapcar #'move-reference (defensive-moves dialogue)))

(defun open-attack-indices (dialogue &key end)
  (let ((moves (subseq (dialogue-plays dialogue) 0 end)))
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

(defun most-recent-open-attack (dialogue &key end)
  (let ((open-attacks (open-attack-indices dialogue :end end)))
    (when open-attacks
      (car open-attacks))))

(defun earliest-open-attack (dialogue &key end)
  (let ((open-attacks (open-attack-indices dialogue :end end)))
    (when open-attacks
      (car (last open-attacks)))))

(defun earliest-open-attack-for-player (dialogue player &key end)
  "The smallest index (starting from 0) of the attacking move by
  PLAYER in DIALOGUE to which there is no response."
  (loop
     with plays = (dialogue-plays dialogue)
     for i from 1 upto (if end end (length plays))
     for move in (cdr plays)
     do
       (when (attacking-move? move)
	 (let ((move-player (move-player move)))
	   (when (eql player move-player)
	     (unless (some #'(lambda (other-move)
			       (and (not (eql (move-player other-move) player))
				    (defensive-move? other-move)
				    (= (move-reference other-move) i)))
			   plays)
	       (return i)))))
     finally
       (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluating rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-structural-rule (rule dialogue &key from-end)
  (funcall (predicate rule) dialogue :final-move-only from-end))

(defun evaluate-particle-rule (rule dialogue)
  (let ((precondition (precondition rule))
	(body (body rule)))
    (loop 
       for move in (dialogue-plays dialogue)
       for turn-number from 0
       do
	 (when (funcall precondition dialogue
			(move-player move)
			turn-number
			(move-statement move)
			(move-stance move)
			(move-reference move))
	     (unless (funcall body dialogue
			      (move-player move)
			      turn-number
			      (move-statement move)
			      (move-stance move)
			      (move-reference move))
	       (return nil)))
       finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions of dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-moves-at-position (dialogue player stance position)
  (let ((result nil)
	(subformulas (proper-subformula-occurrences (initial-statement dialogue)))
	(game-len (dialogue-length dialogue)))
    (if (> position game-len)
	nil
	(dotimes (index position result)
	  (dolist (statement (append subformulas
				     *propositional-symbolic-attacks*))
	    (let* ((provisional-move (make-move player statement stance index))
		   (provisional-extension (provisionally-extend-dialogue dialogue provisional-move)))
	      (when (fast-eval-entire-dialogue provisional-extension)
		(push (list statement index)
		      result))))))))

(defun all-next-moves-at-position (dialogue position)
  (unless (zerop position) ;; not allowed to change the initial move
    (let ((diminished-dialogue (copy-and-truncate-dialogue dialogue position)))
      (append (mapcar #'(lambda (statement-and-reference)
			  (destructuring-bind (statement reference)
			      statement-and-reference
			    (make-move 'p statement 'a reference)))
		      (next-moves-at-position diminished-dialogue 'p 'a position))
	      (mapcar #'(lambda (statement-and-reference)
			  (destructuring-bind (statement reference)
			      statement-and-reference
			    (make-move 'p statement 'd reference)))
		      (next-moves-at-position diminished-dialogue 'p 'd position))
	      (mapcar #'(lambda (statement-and-reference)
			  (destructuring-bind (statement reference)
			      statement-and-reference
			    (make-move 'o statement 'a reference)))
		      (next-moves-at-position diminished-dialogue 'o 'a position))
	      (mapcar #'(lambda (statement-and-reference)
			  (destructuring-bind (statement reference)
			      statement-and-reference
			    (make-move 'o statement 'd reference)))
		      (next-moves-at-position diminished-dialogue 'o 'd position))))))

(defun all-next-proponent-moves-at-position (dialogue position)
  (remove-if-not #'proponent-move?
		 (all-next-moves-at-position dialogue position)))

(defun all-next-opponent-moves-at-position (dialogue position)
  (remove-if-not #'opponent-move?
		 (all-next-moves-at-position dialogue position)))
  
(defun next-moves (dialogue player stance)
  (next-moves-at-position dialogue player stance (dialogue-length dialogue)))

(defun next-attacks (dialogue player)
  (next-moves dialogue player 'a))

(defun next-proponent-attacks (dialogue)
  (next-moves dialogue 'p 'a))

(defun next-opponent-attacks (dialogue)
  (next-moves dialogue 'o 'a))

(defun next-defenses (dialogue player)
  (next-moves dialogue player 'd))

(defun next-proponent-defenses (dialogue)
  (next-moves dialogue 'p 'd))

(defun next-opponent-defenses (dialogue)
  (next-moves dialogue 'o 'd))

(defun next-proponent-moves (dialogue)
  (all-next-proponent-moves-at-position dialogue (dialogue-length dialogue)))

(defun next-opponent-moves (dialogue)
  (all-next-opponent-moves-at-position dialogue (dialogue-length dialogue)))

(defun proponent-wins? (dialogue)
  (and (proponent-move? (last-move dialogue))
       (null (next-moves dialogue 'o 'a))
       (null (next-moves dialogue 'o 'd))))

(defun opponent-wins? (dialogue)
  (and (opponent-move? (last-move dialogue))
       (null (next-moves dialogue 'p 'a))
       (null (next-moves dialogue 'p 'd))))

(defun proponent-loses? (dialogue)
  (not (proponent-wins? dialogue)))

(defun opponent-loses? (dialogue)
  (not (opponent-wins? dialogue)))

(defun freshly-extend-dialogue (dialogue player stance statement reference)
  (let ((new-move (make-move player statement stance reference)))
    (add-move-to-dialogue (copy-dialogue dialogue) new-move)))			  

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
	 (l (setf statement *attack-left-conjunct*))
	 (r (setf statement *attack-right-conjunct*))
	 (d (setf statement *which-disjunct?*))
	 (i (setf statement *which-instance?*)))
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
       (let ((provisional-move (make-move player statement stance index)))
	 (multiple-value-bind (rules-result violated-rules)
	     (eval-entire-dialogue (provisionally-extend-dialogue dialogue provisional-move))
	   (when rules-result
	     (go successful-turn))
	   (msg "At least one of the dialogue rules is violated by your attack:")
	   (dolist (violated-rule violated-rules)
	     (msg "* Rule ~A: ~A" (name violated-rule) (description violated-rule)))
	   (msg "Restarting the move...")
	   (go start-move)))
     successful-turn
       (incf turn-number)
       (add-move-to-dialogue dialogue
			     (make-move player statement stance index))
       (go start-move)
     quit
       (msg "Thanks for playing, I hope you had fun."))
    dialogue))

;;; dialogues.lisp ends hered
