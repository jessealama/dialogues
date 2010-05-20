;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogue-site)

;; Boring server configuration

(defun make-dialogue-backend ()
  (make-backend
   :httpd
   :host "localhost"
   :port 9090))

(defun make-dialogue-server ()
  (make-instance
   'standard-server
   :backend (make-dialogue-backend)))

(defvar *dialogue-server* (make-dialogue-server))

(defun startup-dialogue-server ()
  (startup-server *dialogue-server*))

(defun shutdown-dialogue-server ()
 (shutdown-server *dialogue-server*))

;;;; The definiton of the dialogue

(defclass dialogue-application (standard-application cookie-session-application-mixin)
  ()
  (:default-initargs
   :url-prefix "/"
    :debug-on-error t))

(defvar *dialogue-application* (make-instance 'dialogue-application))

(register-application *dialogue-server* *dialogue-application*)

(defentry-point "" (:application *dialogue-application*)
    ()
  (call 'initial-formula-window))

(defcomponent initial-formula-window (standard-window-component)
  ()
  (:default-initargs
      :title "the game is about to begin"
      :styesheet nil))

(defvar famous-formulas
  `(("Peirce's formula" "peirce-formula" ,peirce-formula)
    ("Excluded middle" "excluded-middle" ,excluded-middle)
    ("Weak excuded middle" "weak-excluded-middle" ,weak-excluded-middle)
    ("Dummet's formula" "dummett-formula" ,dummett-formula)
    ("Double negation introduction" "double-negation-intro" ,double-negation-intro)
    ("Double negation elimination" "double-negation-elim" ,double-negation-elimination)
    ("Markov's formula" "markov-formula" ,markov-formula)
    ("K formula" "k-formula" ,k-formula)
    ("B formula" "b-formula" ,b-formula)
    ("C formula" "c-formula" ,c-formula)
    ("W formula" "w-formulas" ,w-formula)
    ("Scott's formula" "scott-formula" ,scott-formula)
    ("Smetanich's formula" "smetanich-formula" ,smetanich-formula)))

(defcomponent game-viewer (standard-window-component)
  ((player :accessor player
	   :initarg :player
	   :initform nil)
   (statement :accessor statement
	      :initarg :statement
	      :initform nil)
   (stance :accessor stance
	   :initarg :stance
	   :initform nil)
   (reference :accessor reference
	      :initarg :reference
	      :initform nil)
   (game :accessor game
	 :initarg :game)))

(defcomponent game-component ()
  ((game :accessor game
	 :initarg :game
	 :component game-viewer)))

;; OK, now what to do, since we can get all the proper inputs?
;;
;; Well, I say "proper inputs", but we haven't checked that all the
;; inputs are sensible.  The choice of proponent or opponent, and the
;; choice of attack or defend, are correctly set.  But so far we have
;; not checked whether the reference, for example, is a string that
;; represents a number; nor do we check that the input formula is
;; actually a formula.  That needs to be done at some point.  I
;; suppose we should do that when we are accepting the inputs, so that
;; we can approach the task of evaluating the rules with the knowledge
;; that what we are evaluating is sensible.
;;
;; How shall we make sure that the inputs are well-formed?
;; Conditions/restarts?
;;
;; OK, assuming that we have sensible inputs, what do we do?  (By the
;; way, "sensible" is ambiguous.  When entering a formula, we are
;; given strings, I think.  Thus, we might get the string "p".  But in
;; our universe, formulas are not strings (they are either symbols or
;; lists or a certain kind).  The same goes for the reference numbers.
;; Those are given to use as strings, not as numbers.
;;
;; This is what we do in the state machine tagbody, when we have all
;; the right inputs:
;;
;; (multiple-value-bind (rules-result messages)
;;     (evaluate-all-rules rules dialogue player turn-number statement stance index)
;;   (when rules-result
;;     (go successful-turn))
;;   (msg "At least one of the dialogue rules is violated by your attack:")
;;   (dolist (message messages)
;;     (msg "* ~A" message))
;;   (msg "Restarting the move...")
;;   (go start-move))

(defmethod render ((self game-viewer))
  (with-slots (player statement stance reference game)
      self
    (if (and player statement stance reference)
	(progn
	  (<:h1 "Thanks for filling out the form.  I will consider what you said.")
	  (<:p "Here's what you gave me:")
	  (<:ul
	   (<:li "Player: " (<:as-html player))
	   (<:li "Statement: " (<:as-html statement))
	   (<:li "Stance: " (<:as-html stance))
	   (<:li "Reference: " (<:as-html reference))))
	(let (attack-option defend-option
	      proponent-option opponent-option
	      input-reference input-statement)
	  (symbol-macrolet (($take-action (call 'game-viewer
						:player (or player
							    (cond (proponent-option 'p)
								  (opponent-option 'o)
								  (t nil)))
						:statement (or statement
							       input-statement)
						:stance (or stance
							    (if attack-option 'a 
								(when defend-option
								  'd)))
						:reference (or reference
							       input-reference)
						:game game)))
	    (<:h1 "The game so far")
	    (<:div :style "frame-border:1px;"
	      (pretty-print-game game))
	    (<ucw:form :method "POST"
		       :action $take-action
	    (cond ((not stance)
		   (<:p "Enter A or D to attack or defend.")
		   (<:label :for "attack-option" "Attack")
		   (<:label :for "defend-option" "Defend")
		   (<ucw:input :type "radio"
			       :accessor attack-option
			       :id "attack-option"
			       :value "Attack")
		   (<ucw:input :type "radio"
			       :accessor defend-option
			       :id "defend-option"
			       :value "Defend")
		   (<ucw:submit :value "Make a move"
				:action $take-action))
		  ((not player)
		   (<:p "Which player will move?")
		   (<:label :for "proponent-option" "Proponent")
		   (<:label :for "opponent-option" "Opponent")
		   (<ucw:input :type "radio"
			       :accessor proponent-option
			       :id "proponent-action"
			       :value "Proponent")
		   (<ucw:input :type "radio"
			       :accessor opponent-option
			       :id "opponent-action"
			       :value "Opponent")
		   (<ucw:submit :value "Choose sides"
				:action $take-action))
		  ((not reference)
		   (if attack-option
		       (<:p "Which statement do you want to attack?")
		       (<:p "Against which attack do you want to defend?"))
		   (<ucw:input :type "text"
			       :accessor input-reference)
		   (<ucw:submit :value "This is my choice"
				:action $take-action))
		(t ;; we have to get a statement
		 (if attack-option
		     (<:p "How do you want to attack?")
		     (<:p "How do you want to defend?"))
		 (<ucw:input :type "text"
			     :accessor input-statement)
		 (<ucw:submit :value "This is my claim"
			      :action $take-action)))))))))

;; I'm confused about what to do here.  I want the user to indicate,
;; first of all, whether they should attack or defend something.  I
;; think I can do that.  But what component do I call, once I have the
;; information about what kind of stance the user wants to take?  The
;; same component that displays the game?  Do I need to define a
;; new entry point?

(defaction start-game ((self game-component) initial-formula)
  (let ((new-game (make-dialogue initial-formula pqrs-signature)))
    (setf (game game-component) new-game))
  (answer))

(defun pretty-print-game (game)
  (<:table
   (loop with plays = (dialogue-plays game)
      with len = (length plays)
      for play in plays
      for i from 1 upto len
      do
	(with-slots (player statement stance reference)
	    play
	  (<:tr 
	   (<:td (<:as-html player))
	   (<:td (<:as-html statement))
	   (if (= i 1)
	       (<:td)
	       (<:td "[" (<:as-html stance) "," (<:as-html reference) "]")))))))

(defmethod render ((self game-component))
  (let ((game (game self)))
    (<:table
     (loop with plays = (dialogue-plays game)
	   with len = (length plays)
	   for play in plays
	   for i from 1 upto len
	  do
	  (with-slots (player statement stance reference)
	      play
	    (<:tr 
	     (<:td (<:as-html player))
	     (<:td (<:as-html statement))
	     (if (= i 1)
		 (<:td)
		 (<:td "[" (<:as-html stance) "," (<:as-html reference) "]"))))))))

(defmethod render ((self initial-formula-window))
  (let (input-formula selected-formula)
    (symbol-macrolet (($take-action (call 'game-viewer
					  :game (make-dialogue (if (empty-string? input-formula)
								   selected-formula
								   markov-formula)
							       pqrs-propositional-signature))))
      (<:h1 "It's your turn")
      (<ucw:form :method "POST"
		 :action $take-action
      (<:p (<:label :for "input-formula" "Enter a formula in the text box")
	   (<ucw:input :type "text" :accessor input-formula :id "input-formula")
	   (<:label :for "selected-formula" "or select a famous formula from the menu")
	   (<ucw:select :id "selected-formula" 
			:size 1 
			:accessor selected-formula
	     (dolist (famous-formula famous-formulas)
	       (destructuring-bind (long-name short-name formula)
		   famous-formula
		 (declare (ignore short-name))
		 (<ucw:option :value formula (<:as-html long-name))))))
      (<:p
       (<:as-html "If the text box is not empty, its contents will be the initial formula.  If the text box is empty, then the selected \"famous formula\" will be used."))
      (<:p
       (<ucw:submit :action $take-action
		    :value "Let's play"))))))

;;; ucw-site.lisp ends here