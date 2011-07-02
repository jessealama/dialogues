;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogues)

(defparameter standard-rulesets
  (list d-dialogue-rules
	e-dialogue-rules
	classical-dialogue-rules
	nearly-classical-dialogue-rules))

(defparameter standard-structural-rules
  (list rule-d10
	rule-d11
	rule-d12
	rule-d13
	rule-e))

(defparameter exotic-structural-rules
  (list rule-d10-literal
	rule-d11-most-recent-attack
	rule-d11-queue
	rule-d13-symmetric
	rule-d12-two-times
	rule-d13-two-times
	rule-d13-three-times))

(defparameter available-rulesets
  (append
   ;; main rulesets
   (list d-dialogue-rules 
	 e-dialogue-rules
	 classical-dialogue-rules
	 nearly-classical-dialogue-rules)
   ;; experimental rulesets
   (sort (list d-dialogue-rules-queue
	       e-dialogue-rules-queue
	       conjectural-classical-dialogue-rules
	       d-dialogue-rules-minus-d10
	       d-dialogue-rules-minus-d11
	       e-dialogue-rules-minus-d11
	       d-dialogue-rules-minus-d12
	       e-dialogue-rules-minus-d12
	       d-dialogue-rules-symmetric-d13
	       d-dialogue-rules-literal-d10
	       e-dialogue-rules-literal-d10
	       only-particle-rules
	       particle-rules+d10
	       particle-rules+d11
	       particle-rules+d12
	       particle-rules+d13
	       particle-rules+e
	       sara-ad-hoc-rules
	       sara-ad-hoc-rules-2)
	 #'lex<
	 :key #'description)))

(defparameter available-heuristics
  (list proponent-no-repeats))

(defclass ruleset-component ()
  ((ruleset :initarg :ruleset
	    :initform nil
	    :accessor ruleset
	    :type (or null ruleset))
   (extra-rules
    :initarg :extra-rules
    :accessor extra-rules
    :initform nil
    :type list)
   (heuristics
    :initarg :heuristics
    :initform nil
    :type list
    :accessor heuristics
    :documentation "Heuristic rules that, in addition to RULESET, are also in effect.")))

(defclass game-component ()
  ((game :accessor game
	 :initarg :game
	 :initform nil
	 :type (or (eql nil) dialogue))))

(defun render-available-heuristics ()
  (dolist (heuristic available-heuristics)
    (let ((desc (description heuristic)))
      (<:input :type "checkbox"
	       :value desc))))

(defun render-heuristics (heuristic-list)
  (if (null heuristic-list)
      (<:em "(none)")
      (<:ul
       (dolist (heuristic heuristic-list)
	 (<:li
	  (<:strong (<:as-html (name heuristic)))
	  ": "
	  (<:as-html (description heuristic)))))))

(defclass play-style-component ()
  ((play-style :accessor play-style
	       :initarg :play-style
	       :initform nil)))

(defentry-point "" (:application *dialogue-application*)
    ()
    (call 'initial-formula-window))

(defcomponent initial-formula-window (standard-window-component)
  ()
  (:default-initargs
      :title "explore dialogical logic with lorenzen dialogue games"
      :doctype yaclml:+xhtml-strict-doctype+
      :body
      (make-instance 'tabbed-pane
		     :current-component-key "play a game"
		     :key-test #'string=
		     :contents `(("play a game" . ,(make-instance 'start-game-component))
				 ("about dialogical logic" . ,(make-instance 'about-component))
				 ("about this site" . ,(make-instance 'about-this-site-component))
				 ("contact" . ,(make-instance 'contact-page))))))

(defcomponent turn-editor (game-component play-style-component ruleset-component)
  ())

(defcomponent turn-evaluator (game-component play-style-component ruleset-component)
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
	      :initform nil)))

(defmethod render ((self turn-evaluator))
  (with-slots (player statement stance reference game play-style heuristics extra-rules)
      self
    (let ((game-len (dialogue-length game)))
    (if (and player statement stance reference)
	(multiple-value-bind (rules-result violated-rules)
	    (eval-provisional-dialogue game player statement stance reference)
	  (if rules-result
	      (progn
		(<:h1 "Success!")
		(<:p "Your move was accepted. "
		     (<ucw:a :action 
			     (call 'turn-editor
				   :play-style (play-style self)
				   :game (add-move-to-dialogue-at-position
					  game
					  (make-move player
						     statement
						     stance
						     reference)
					  game-len))
			     "Proceed") "."))
	      (progn
		(<:h1 "Problem!")
		(<:p "The game at this point:")
		(unless (zerop (dialogue-length game))
		  (let ((open-attacks (open-attack-indices game)))
		    (<:table 
		     (<:colgroup :span "2" :align "center")
		     (<:colgroup :span "2" :align "left")
		     (<:thead
		      (<:th "Move")
		      (<:th "Player")
		      (<:th "Assertion")
		      (<:th "Stance, Reference")
		      (<:th "Notes"))
		     (<:tbody
		      (loop with plays = (dialogue-plays game)
			 with len = (length plays)
			 for play in plays
			 for i from 0 upto len
			 do
			   (render-move-in-game 
			    play game i
			    :indicate-alternatives nil
			    :heuristics heuristics
			    :extra-rules extra-rules
			    :play-style play-style
			    :attack-is-closed (not (member i open-attacks 
							   :test #'=))))
		      (<:tr :style "background-color:#FF3333;"
			    (<:td :align "left" (<:as-html game-len))
			    (<:td :align "center" (<:strong (<:as-html player)))
			    (<:td :align "left" (render statement))
			    (<:td :align "right"
				  (<:as-html "[" stance "," reference "]"))
			    (<:td)))
		    (<:tfoot
		      (<:tr
		       (<:td :align "center"
			     :colspan "5"
			     (<:em "Rows in " (<:span :style "background-color:#CCCCCC"
						      "grey")
				   " are closed attacks; rows in "
				   (<:span :style "background-color:#CCCCFF"
					   "blue")
				   " are open attacks.  (Defensive moves are not colored, nor is the initial move, since it is neither an attack nor a defense.)")))))))
		(<:p "At least one of the dialogue rules is violated by your proposed move:")
		(<:ul
		 (dolist (violated-rule violated-rules)
		   (<:li "Rule " (<:as-html (name violated-rule))
			 (<:br)
			 "Description: " (<:as-html (description violated-rule)))))
		(<ucw:form :method "post"
			   :action (call 'turn-editor 
					 :game game
					 :play-style (play-style self))
		  (<:p "You must edit your move; please try again.")
		  (<:submit :value "Go back and edit the move")))))))))

(defun render-attack (attack player game position)
  (destructuring-bind (statement reference)
      attack
    (let* ((statement-str (render-plainly statement))
	   (title (format nil "Attack move ~d by asserting ~A"
			  reference
			  statement-str)))
      (<ucw:a
       :action (add-attack-to-dialogue-at-position game
						   player
						   statement
						   reference
						   position)
       :title title
       "Attack move " (<:as-html reference) " by asserting " (render statement)))))

(defun render-proponent-attack (attack game position)
  (render-attack attack 'p game position))

(defun render-opponent-attack (attack game position)
  (render-attack attack 'o game position))

(defun render-defense (defense player game position)
  (destructuring-bind (statement reference)
      defense
    (let* ((statement-str (render-plainly statement))
	   (title (format nil "Defend against the attack of move ~d by asserting ~A" 
			  reference
			  statement-str)))
      (<ucw:a
       :action (add-defense-to-dialogue-at-position game
						    player
						    statement
						    reference
						    position)
       :title title
       "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement)))))

(defun render-proponent-defense (defense game position)
  (render-defense defense 'p game position))

(defun render-opponent-defense (defense game position)
  (render-defense defense 'o game position))

(defun render-attacks (game)
  (let ((game-len (dialogue-length game)))
  (labels ((render-multiple-attacks (attacks player)
	     (dolist (attack attacks)
	       (<:tr
		(<:td (render-attack attack player game game-len))))))
  (let* ((next-proponent-attacks (next-attacks game 'p))
	 (next-opponent-attacks (next-attacks game 'o))
	 (num-proponent-attacks (length next-proponent-attacks))
	 (num-opponent-attacks (length next-opponent-attacks)))
    (<:table
     (<:thead
      (<:th "Attacks for Proponent")
      (<:th "Attacks for Opponent"))
     (<:tbody
      (multiple-value-bind (last-man-standing last-man-tail)
	  (map-initial-pairs next-proponent-attacks
			     next-opponent-attacks
			     #'(lambda (proponent-attack opponent-attack)
				 (<:tr
				  (<:td (render-proponent-attack proponent-attack game game-len))
				  (<:td (render-opponent-attack opponent-attack game game-len)))))
	(if (zerop last-man-standing)
	    (if (null last-man-tail)
		(<:tr
		 (<:td (<:em "(no attacks are available)"))
		 (<:td (<:em "(no attacks are available)")))
		(if (zerop num-opponent-attacks)
		    (let ((first-remaining-attack (car last-man-tail)))
		      (<:tr
		       (<:td (render-proponent-attack first-remaining-attack game game-len))
		       (<:td :rowspan (length last-man-tail)
			     (<:em "(no attacks are available)")))
		      (render-multiple-attacks (cdr last-man-tail) 'p))
		    (render-multiple-attacks last-man-tail 'p)))
	    (if (null last-man-tail)
		(<:tr
		 (<:td (<:em "(no attacks are available)"))
		 (<:td (<:em "(no attacks are available)")))
		(if (zerop num-proponent-attacks)
		    (let ((first-remaining-attack (car last-man-tail)))
		      (<:tr
		       (<:td :rowspan (length last-man-tail)
			     (<:em "(no attacks are available)"))
		       (<:td (render-opponent-attack first-remaining-attack game game-len)))
		      (render-multiple-attacks (cdr last-man-tail) 'o))
		    (render-multiple-attacks last-man-tail 'o)))))))))))

(defun render-defenses (game)
  (let ((game-len (dialogue-length game)))
  (labels
      ((render-multiple-defenses (defenses player)
	 (dolist (defense defenses)
	   (<:tr
	    (<:td (render-defense defense player game game-len))))))
  (let* ((next-proponent-defenses (next-defenses game 'p))
	 (next-opponent-defenses (next-defenses game 'o))
	 (num-proponent-defenses (length next-proponent-defenses))
	 (num-opponent-defenses (length next-opponent-defenses)))
    (<:table
     (<:thead
      (<:th "Defenses for Proponent")
      (<:th "Defenses for Opponent"))
     (<:tbody
      (multiple-value-bind (last-man-standing last-man-tail)
	  (map-initial-pairs next-proponent-defenses
			     next-opponent-defenses
			     #'(lambda (proponent-defense opponent-defense)
				 (<:tr
				  (<:td (render-proponent-defense proponent-defense game game-len))
				  (<:td (render-opponent-defense opponent-defense game game-len)))))
	(if (zerop last-man-standing)
	    (if (null last-man-tail)
		(<:tr
		 (<:td (<:em "(no defenses are available)"))
		 (<:td (<:em "(no defenses are available)")))
		(if (zerop num-opponent-defenses)
		    (let ((first-remaining-defense (car last-man-tail)))
		      (<:tr
		       (<:td (render-proponent-defense first-remaining-defense game game-len))
		       (<:td :rowspan (length last-man-tail)
			     (<:em "(no defenses are available)")))
		      (render-multiple-defenses (cdr last-man-tail) 'p))
		    (render-multiple-defenses last-man-tail 'p)))
	    (if (null last-man-tail)
		(<:tr
		 (<:td (<:em "(no defenses are available)"))
		 (<:td (<:em "(no defenses are available)")))
		(if (zerop num-proponent-defenses)
		    (let ((first-remaining-defense (car last-man-tail)))
		      (<:tr
		       (<:td :rowspan (length last-man-tail)
			     (<:em "(no defenses are available)"))
		       (<:td (render-opponent-defense first-remaining-defense game game-len)))
		      (render-multiple-defenses (cdr last-man-tail) 'o))
		    (render-multiple-defenses last-man-tail 'o)))))))))))

(defun render-available-moves (game)
  (<:p "Below is a description of all available moves for both
Proponent and Opponent.")
  (render-attacks game)
  (<:br)
  (render-defenses game)
  (<:p (<:em "Note:") " The list of moves available to each player in
the following list is determined by the set of proper subformulas of
all formulas played so far. When using the usual rules for
propositional dialogue games (Felscher's D-rules and E-rules, for
example), this is sufficient: every assertion of a dialogue game is a
subformula of the initial formula, or is one of the symbolic
attacks (" (<:as-is "&and;<sub>L</sub>, &and;<sub>R</sub>, and
?)") ". If one modifies the rules, this structural fact about formulas
occuring in a dialogue game may no longer hold, so that the list below
may no longer be an exhaustive enumeration of all formulas that can be
asserted in the next move."))

(defun render-manual-move-entry-form (game play-style)
  (let (input-statement player-option reference-option selected-symbolic-attack stance-option)
  (symbol-macrolet 
      (($take-action 
	(let (new-statement)
	  (if (empty-string? input-statement)
	      (setf new-statement selected-symbolic-attack)
	      (setf new-statement
		    (ucw-handler-case (parse-formula input-statement)
		      (malformed-formula-error
		       () 
		       (call 'formula-corrector
			     :text input-statement
			     :signature (dialogue-signature game))))))
	  (call 'turn-evaluator
		:play-style play-style
		:player player-option
		:stance stance-option
		:reference reference-option
		:statement new-statement
		:game game))))
    (let ((game-len (dialogue-length game)))
      (<:p "The list in the previous section shows all moves that
could be made, by either player, that adhere to the dialogue rules;
follow the links there to make the corresponding moves.  Here, you can
enter a move manually.  If the move you enter is different from any of
the moves in the previous section, and if you are using some standard
set of dialogue rules (such as Felscher's D-rules or E-rules), then it
will be inadmissible and you will see which of the rules are violated by
your move.  Entering moves manully is a good way to explore the
meaning of the dialogue rules.")
      (<ucw:form :method "post"
		 :action $take-action
      (<:table :style "border:1px solid;"
	       (<:tr 
		(<:td "Which player will move?")
		(<:td (<ucw:select :accessor player-option
				   :size 1
		        (<ucw:option :value 'p "Proponent")
			(<ucw:option :value 'o "Opponent"))))
	       (<:tr
		(<:td "Attack or defend?")
		(<:td (<ucw:select :accessor stance-option
				   :size 1
		        (<ucw:option :value 'a "Attack")
			(<ucw:option :value 'd "Defend"))))
	       (<:tr
		(<:td "Choose the statement to which the selected player is responding.")
		(<:td (<ucw:select :accessor reference-option
				   :size 1
		        (loop for i from 0 upto (1- game-len)
			   do (<ucw:option :value i (<:as-html i))))))
	       (<:tr
		(<:td
		 "What do you want to assert? Enter a formula or choose a symbolic attack.  (If you enter a formula, it will be your proposed assertion; otherwise, the displayed symbolic attack in the menu will be your move.)")
		(<:td (<ucw:input :type "text"
				  :id "input-statement"
				  :accessor input-statement)
		      (<ucw:select :accessor selected-symbolic-attack
				   :size 1
			  (<ucw:option :value *attack-left-conjunct*
				       "Attack the left conjunct")
			  (<ucw:option :value *attack-right-conjunct*
				       "Attack the right conjunct")
			  (<ucw:option :value *which-disjunct?*
				       "Request that a disjunct be chosen"))))
	       (<:caption :style "caption-side:bottom;"
	         (<ucw:submit :value "Make a move"
			      :action $take-action))))))))

(defun render-proponent-attack-against-random-opponent (attack game position)
  (destructuring-bind (statement reference)
      attack
    (let* ((game-after-attack
	    (add-attack-to-dialogue-at-position (copy-dialogue game)
						'p
						statement
						reference
						position))
	   (statement-str (render-plainly statement))
	   (title (format nil "Attack move ~d by asserting ~A"
			  reference
			  statement-str))			  
	   (next-opponent-attacks (next-attacks game-after-attack 'o))
	   (next-opponent-defenses (next-defenses game-after-attack 'o))
	   (all-opponent-moves (append next-opponent-attacks
				       next-opponent-defenses)))
	(if (null all-opponent-moves)
	    (<ucw:a :action (call 'turn-editor
				  :play-style 'play-as-proponent-random-opponent
				  :game game-after-attack)
		    :title title
		    "Attack move " (<:as-html reference) " by asserting " (render statement) " (you would win the game)")
	    (<ucw:a :action 
		    (let ((random-move (random-element all-opponent-moves)))
		      (destructuring-bind (statement reference)
			  random-move
			(if (member random-move next-opponent-attacks)
			    (call 'turn-editor
				  :play-style 'play-as-proponent-random-opponent
				  :game
				  (add-move-to-dialogue-at-position game-after-attack
								    (make-move 'o statement 'a reference)
								    (1+ position)))
			    (call 'turn-editor
				  :play-style 'play-as-proponent-random-opponent
				  :game
				  (add-move-to-dialogue-at-position game-after-attack
								    (make-move 'o statement 'd reference)
								    (1+ position))))))
		    :title title
		    "Attack move " (<:as-html reference) " by asserting " (render statement))))))

(defun render-opponent-attack-against-random-proponent (attack game position)
  (destructuring-bind (statement reference)
      attack
    (let* ((game-after-attack
	    (add-attack-to-dialogue-at-position (copy-dialogue game)
						'o
						statement
						reference
						position))
	   (statement-str (render-plainly statement))
	   (title (format nil "Attack move ~d by asserting ~A" 
			  reference
			  statement-str))
	   (next-proponent-attacks (next-attacks game-after-attack 'p))
	   (next-proponent-defenses (next-defenses game-after-attack 'p))
	   (all-proponent-moves (append next-proponent-attacks
					next-proponent-defenses)))
	(if (null all-proponent-moves)
	    (<ucw:a :action (call 'turn-editor
				  :play-style 'play-as-opponent-random-proponent
				  :game game-after-attack)
		    :title title
		    "Attack move " (<:as-html reference) " by asserting " (render statement) " (you would win the game)")
	    (<ucw:a :action 
		    (let ((random-move (random-element all-proponent-moves)))
		      (destructuring-bind (statement reference)
			  random-move
			(if (member random-move next-proponent-attacks)
			    (call 'turn-editor
				  :play-style 'play-as-opponent-random-proponent
				  :game
				  (add-move-to-dialogue-at-position game-after-attack
								    (make-move 'p statement 'a reference)
								    (1+ position)))
			    (call 'turn-editor
				  :play-style 'play-as-opponent-random-proponent
				  :game
				  (add-move-to-dialogue-at-position game-after-attack
								    (make-move 'p statement 'd reference)
								    (1+ position))))))
		    :title title
		    "Attack move " (<:as-html reference) " by asserting " (render statement))))))

(defun render-opponent-defense-against-random-proponent (defense game position)
  (destructuring-bind (statement reference)
      defense
    (let* ((game-after-defense
	    (add-defense-to-dialogue-at-position (copy-dialogue game)
						 'o
						 statement
						 reference
						 position))
	     (statement-str (render-plainly statement))
	     (title (format nil "Defend against the attack of move ~d by asserting ~A"
			    reference
			    statement-str))
	     (next-proponent-attacks (next-attacks game-after-defense 'p))
	     (next-proponent-defenses (next-defenses game-after-defense 'p))
	     (all-proponent-moves (append next-proponent-attacks
					  next-proponent-defenses)))
	(if (null all-proponent-moves)
	    (<ucw:a :action (call 'turn-editor
				  :play-style 'play-as-opponent-random-proponent
				  :game game-after-defense)
		    :title title
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement) " (you would win the game)")
	    (<ucw:a :action 
		    (let ((random-move (random-element all-proponent-moves)))
		      (destructuring-bind (statement reference)
			  random-move
			(if (member random-move next-proponent-attacks)
			    (call 'turn-editor
				  :style 'play-as-opponent-random-proponent
				  :game
				  (add-move-to-dialogue-at-position game-after-defense
								    (make-move 'p statement 'a reference)
								    (1+ position)))
			    (call 'turn-editor
				  :play-style 'play-as-opponent-random-proponent
				  :game
				  (add-move-to-dialogue-at-position game-after-defense
								    (make-move 'p statement 'd reference)
								    (1+ position))))))
		    :title title
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement))))))

(defun render-proponent-defense-against-random-opponent (defense game position)
  (destructuring-bind (statement reference)
      defense
    (let* ((game-after-defense
	    (add-defense-to-dialogue-at-position (copy-dialogue game)
						 'p
						 statement
						 reference
						 position))
	   (statement-str (render-plainly statement))
	   (title (format nil "Defend against the attack of move ~d by asserting ~A"
			  reference
			  statement-str))
	   (next-opponent-attacks (next-attacks game-after-defense 'o))
	   (next-opponent-defenses (next-defenses game-after-defense 'o))
	   (all-opponent-moves (append next-opponent-attacks
				       next-opponent-defenses)))
	(if (null all-opponent-moves)
	    (<ucw:a :action (call 'turn-editor
				  :play-style 'play-as-proponent-random-opponent
				  :game game-after-defense)
		    :title title
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement) " (you would win the game)")
	    (<ucw:a :action 
		    (let ((random-move (random-element all-opponent-moves)))
		      (destructuring-bind (statement reference)
			  random-move
			(if (member random-move next-opponent-attacks)
			    (call 'turn-editor
				  :play-style 'play-as-proponent-random-opponent
				  :game (add-move-to-dialogue-at-position game-after-defense
									  (make-move 'o statement 'a reference)
									  (1+ position)))
			    (call 'turn-editor
				  :play-style 'play-as-proponent-random-opponent
				  :game (add-move-to-dialogue-at-position game-after-defense
									  (make-move 'o statement 'd reference)
									  (1+ position))))))
		    :title title
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement))))))

(defun render-proponent-attacks-with-random-opponent (game)
  (let* ((game-len (dialogue-length game))
	 (attacks (next-attacks game 'p)))
    (<:table
     (<:thead
      (<:th "Attacks for Proponent"))
     (<:tbody
      (if (null attacks)
	  (<:tr (<:td (<:em "(no attacks are available)")))
	   (dolist (attack attacks)
	     (<:tr
	      (<:td (render-proponent-attack-against-random-opponent attack
								     game
								     game-len)))))))))

(defun render-proponent-defenses-with-random-opponent (game)
  (let* ((game-len (dialogue-length game))
	 (defenses (next-defenses game 'p)))
    (<:table
     (<:thead
      (<:th "Defenses for Proponent"))
     (<:tbody
      (if (null defenses)
	  (<:tr (<:td (<:em "(no defenses are available)")))
	   (dolist (defense defenses)
	     (<:tr
	      (<:td (render-proponent-defense-against-random-opponent defense
								     game
								     game-len)))))))))

(defun render-opponent-attacks-with-random-proponent (game)
  (let* ((game-len (dialogue-length game))
	 (attacks (next-attacks game 'o)))
    (<:table
     (<:thead
      (<:th "Attacks for Opponent"))
     (<:tbody
      (if (null attacks)
	  (<:tr (<:td (<:em "(no attacks are available)")))
	   (dolist (attack attacks)
	     (<:tr
	      (<:td (render-opponent-attack-against-random-proponent attack
								     game
								     game-len)))))))))

(defun render-opponent-defenses-with-random-proponent (game)
  (let* ((game-len (dialogue-length game))
	 (defenses (next-defenses game 'o)))
    (<:table
     (<:thead
      (<:th "Defenses for Opponent"))
     (<:tbody
      (if (null defenses)
	  (<:tr (<:td (<:em "(no defenses are available)")))
	   (dolist (defense defenses)
	     (<:tr
	      (<:td (render-opponent-defense-against-random-proponent defense
								     game
								     game-len)))))))))

(defun render-available-proponent-moves (game)
  (<:p "Below is a description of all available moves for Proponent.")
  (render-proponent-attacks-with-random-opponent game)
  (<:br)
  (render-proponent-defenses-with-random-opponent game)
  (<:p (<:em "Note:") " The list of moves available to each player in
the following list is determined by the set of subformulas of all
formulas played so far. When using the usual rules for propositional
dialogue games (Felscher's D-rules and E-rules, for example), this is
sufficient: every assertion of a dialogue game is a subformula of the
initial formula, or is one of the symbolic
attacks (" (<:as-is "&and;<sub>L</sub>, &and;<sub>R</sub>, and
?)") ". If one modifies the rules, this structural fact about formulas
occuring in a dialogue game may no longer hold, so that the list below
may no longer be an exhaustive enumeration of all formulas that can be
asserted in the next move."))

(defun render-available-opponent-moves (game)
  (<:p "Below is a description of all available moves for Proponent.")
  (render-opponent-attacks-with-random-proponent game)
  (<:br)
  (render-opponent-defenses-with-random-proponent game)
  (<:p (<:em "Note:") " The list of moves available to each player in
the following list is determined by the set of subformulas of all
formulas played so far. When using the usual rules for propositional
dialogue games (Felscher's D-rules and E-rules, for example), this is
sufficient: every assertion of a dialogue game is a subformula of the
initial formula, or is one of the symbolic
attacks (" (<:as-is "&and;<sub>L</sub>, &and;<sub>R</sub>, and
?)") ". If one modifies the rules, this structural fact about formulas
occuring in a dialogue game may no longer hold, so that the list below
may no longer be an exhaustive enumeration of all formulas that can be
asserted in the next move."))

(defun render-rewind-form (game play-style)
  (let (rewind-point)
  (let ((game-len (dialogue-length game)))
    (<:p "Select a number between 1 and the one less than the
current turn number.  Moves of the game after the selected number will
be discarded, and the state of the game will be rewound so that the
current turn number is the selected one.")
    (<ucw:form :method "post"
	       :action (call 'turn-editor
			     :game (truncate-dialogue game
						      rewind-point)
			     :play-style play-style)
      (<:table
       (<:tr
	(<:td
	 (<ucw:select :size 1
		      :accessor rewind-point
	   (loop for i from 1 upto (1- game-len)
	      do (<ucw:option :value i (<:as-html i)))))
	(<:td
	 (<:submit :value "Rewind the game to this turn"))))))))

(defcomponent rule-editor (game-component ruleset-component play-style-component)
  ())

(defmethod render ((self rule-editor))
  (let* ((game (game self))
	 (ruleset (dialogue-rules game))
	 (rules (rules ruleset))
	 (selected-ruleset))
    (let (indices-and-violated-rules)
      (loop 
	 for turn-number from 1 upto (dialogue-length game)
	 do
	   (let ((truncated (copy-and-truncate-dialogue game turn-number)))
	     (multiple-value-bind (ok? violators)
		 (eval-entire-dialogue truncated :structural-rules-from-end t)
	       (unless ok?
		 (push (cons (1- turn-number) violators)
		       indices-and-violated-rules))))
	 finally
	   (setf indices-and-violated-rules
		 (reverse indices-and-violated-rules)))
    (unless (null indices-and-violated-rules)
      (<:h1 "The game is incoherent with respect to the current ruleset")
      (<:p "With the current ruleset, the game is incoherent.  Here is
      a listing of the game, annotated with the violating moves:")
      (<:div :style "border:1px solid;"
        (render-game game
		     :moves-to-highlight (mapcar #'car indices-and-violated-rules)
		     :heuristics (heuristics self)
		     :extra-rules (extra-rules self)))
      (<:p "At least one move of the game is violated with these rules:")
      (<:ul 
       (dolist (index-and-violators indices-and-violated-rules)
	 (destructuring-bind (index . violated-rules)
	     index-and-violators
	   (<:li "Move " (<:as-html index) " violates these rules:"
	     (<:ul
	      (dolist (rule violated-rules)
		(<:li "Name: " (<:as-html (name rule))
		      (<:br)
		      "Description: " (<:as-html (description rule))))))))))
    (<:h1 "The current ruleset")
    (<:p "A concise description of the ruleset currently in force in the game:")
    (<:blockquote
     (<:as-html (description ruleset)))
    (when (null indices-and-violated-rules)
      (<:p "Since the game is well-formed with the ruleset currently
	in force, you are welcome to " (<ucw:a :action (answer ruleset)
					       "go back and continue playing the game")))
    (<:p "You are welcome to look more carefully at the rules and edit
    them, if you wish.  Here are the rules that constitute the current
    ruleset:")
    (if (null rules)
	(<:blockquote
	 "(none)")
	(<:table
	 (<:thead
	  (<:tr
	   (<:th "Name")
	   (<:th "Description")))
	 (<:tbody
	  (dolist (rule rules)
	    (with-slots (name description) 
		rule
	      (<:tr
	       (<:td :align "right" (<:as-html name))
	       (<:td :align "left" (<:as-html description))))))))
    (<ucw:form :action (let ((test-dialogue (make-instance 'dialogue
							   :rules (if (null selected-ruleset)
								      (dialogue-rules dialogue)
								      selected-ruleset)
							   :plays (dialogue-plays game)
							   :signature (dialogue-signature game))))
			 (if (eval-entire-dialogue test-dialogue)
			     (answer (dialogue-rules test-dialogue))
			     (answer (call 'rule-editor
					   :game test-dialogue))))
      (<:p "You may " (<ucw:a :action (answer (dialogue-rules game))
			      "proceed without changing the current ruleset")
	   (<:br)
	   " or change the ruleset to: "
	   (<ucw:select :accessor selected-ruleset
	      (dolist (ruleset available-rulesets)
		(<ucw:option :value ruleset
			     (<:as-html (description ruleset)))))
	   (<:submit :value "Use this ruleset instead of the current one"))))))

(defcomponent winning-play-searcher (game-component play-style-component)
  ((depth :initarg :depth
	  :accessor depth)
   (queue :initarg :queue
	  :accessor queue
	  :initform nil)
   (success :initarg :success
	    :accessor success
	    :initform nil)))

(defmethod render ((self winning-play-searcher))
  (with-slots (game depth play-style queue success)
      self
    (multiple-value-bind (success? result more-nodes)
	(bounded-dialogue-search-bfs (dialogue-rules game)
				     (initial-statement game)
				     (dialogue-signature game)
				     depth
				     game
				     queue)
      (cond (success?
	     (let ((winning-play (node-state result)))
	       (<:h1 "Success")
	       (<:p "Here is a continuation of the initial game that leads to a win no more than " (<:as-html depth) " " (if (= depth 1) (<:as-is "move") (<:as-is "moves")) " beyond the end of the initial game:")
	       (<:div :style "border:1px solid"
	         (render-game winning-play
			      :extra-rules (extra-rules self)
			      :heuristics (heuristics self)))))
	     ((null result)
	      (<:h1 "Ouch!")
	      (<:p "Not only is there is no winning play that continues from the game above no more than " (<:as-html depth) " " (if (= depth 1) "move" "moves") ", there is actually " (<:em "no") " winning play at all that extends the initial game."))
	    (t ;; :cut-off
	     (if success
		 (progn
		   (<:h1 "No more winning plays")
		   (<:p "No more winning plays are available, given the depth limit."))
		 (progn
		   (<:h1 "Cut off!")
		   (<:p "No winning play "
			(<:as-is "&le; ")
			(<:as-html depth) " " 
			(if (= depth 1)
			    (<:as-is "move")
			    (<:as-is "moves"))
			" from the end of the current game was found.  The search was terminated because of the depth limit.")))))
      (<:fieldset
       (if (empty-queue? more-nodes)
	   (<:p "The search was exhaustive; there are no more options left to consider and there would be none even if the depth restriction were dropped.")
	   (if (eq result :cut-off)
	       (<:p "When the search terminated, there were still possible moves left to consider; since the search was terminated by the depth cutoff, it's possible that there are " (when success (<:as-is "further ")) "winning plays that extend the initial game, but all of them are too deep.")
	       (<ucw:form :method "post"
			  :action (call 'winning-play-searcher
					:game game
					:depth depth
					:play-style play-style
					:queue more-nodes
					:success t)
			  (<:p "When the search terminated, there were still possible moves left to consider; there may be further winning plays different from this one.")
			  (<:submit :value "Interactively search for another winning play"))))
       (<ucw:form :method "post"
		  :action (call 'turn-editor
				:game game
				:play-style play-style)
         (<:submit :value "Go back to the original game"))
       (<ucw:form :method "post"
		  :action (call 'start-game-component)
         (<:submit :value "Quit"))))))

(defun render-move-at-depth-as-table (move depth)
  (with-slots (player statement stance reference)
      move
    (<:table
     (<:tr
      (<:td :align "left"
	    (<:as-html depth))
      (<:td :align "center"
	    (<:strong (<:as-html player)))
      (<:td :align "left"
	    (render statement))
      (<:td :align "left"
	    (unless (initial-move? move)
	      (if (attacking-move? move)
		  (<:as-html "[A," reference "]")
		  (<:as-html "[D," reference "]"))))))))

(defun render-final-move-as-table-row (node)
  (let* ((game (node-state node))
	 (move (last-move game)))
    (render-move-at-depth-as-table move (node-depth node))))

(defun render-final-move-with-padding (node width)
  (let ((cleft-point (/ width 2)))
    (loop
       for i from 1 upto (floor cleft-point)
       do
	 (<:td))
    (<:td (render-final-move-as-table-row node))
    (loop
       for i from (1+ (ceiling cleft-point)) upto width
       do
	 (<:td))))

(defun render-node-as-table-row (node)
  (let* ((game (node-state node))
	 (move (last-move game))
	 (depth (node-depth node)))
    (with-slots (player statement stance reference)
	move
      (<:tr
       (<:td :align "left"
	     (<:as-html depth))
       (<:td :align "center"
	     (<:as-html player))
       (<:td :align "left"
	     (render statement))
       (<:td :align "left"
	     (unless (initial-move? move)
	       (if (attacking-move? move)
		   (<:as-html "[A," reference "]")
		   (<:as-html "[D," reference "]"))))))))
  
(defun render-segment-from-to-with-padding-as-row (begin end padding)
  "Given search tree nodes BEGIN and END, render a single HTML table
  row representing the dialogue from BEGIN to END.  The row will
  contain 2*PADDING + 1 columns; PADDING empty columns will be put on
  the left and the right of the sequence.  It is assumed that there is
  a path from BEGIN to END; the path is constrcted simply taking
  unique successors, starting at BEGIN, until we reach END.  The moves
  of the game between BEGIN and END will be put into a single HTML
  table element."
  (<:tr
   (dotimes (i padding)
     (<:td))
   (<:td :align "center"
     (<:table
      (let ((current-node begin))
	(until (eq current-node end)
	  (render-node-as-table-row current-node)
	  (setf current-node (first (node-successors current-node))))
	(render-node-as-table-row end))))
   (dotimes (i padding)
     (<:td))))

(defun render-win-searcher (game play-style)
  (let (search-depth)
  (<:p "From the current state of the game, you can search for a " (<:em "winning play") " or a " (<:em "winning strategy") ".  A winning play is a sequence of moves that leads to a win for Proponent, whereas a winning strategy is a way of playing the game in such a way that Proponent can win the game no matter what Opponent does. (Winning strategies are generally not sequences; they are more complicated objects than winning plays.)")
  (<:p "Select the number of moves beyond the end of the current game that should be searched, and choose the kind of object for which to search. " (<:b "Note:") " generally, the greater the depth, the more time it will take to compute an answer; be patient.")
  (<ucw:form :action (call 'winning-play-searcher
			   :depth search-depth
			   :game game
			   :play-style play-style)
   (<:p "Number of moves: "
	(<ucw:select :size 1
		     :accessor search-depth
	  (dotimes (i max-search-depth)
	    (<ucw:option :value (1+ i) (<:as-html (1+ i)))))
	" "
	(<:submit :value "Search for a winning play")))
  (<ucw:form :action (call 'winning-strategy-searcher
			   :depth search-depth
			   :game game
			   :play-style play-style)
   (<:p "Number of moves: "
	(<ucw:select :size 1
		     :accessor search-depth
	  (dotimes (i max-search-depth)
	    (<ucw:option :value (1+ i) (<:as-html (1+ i)))))
	" "
	(<:submit :value "Search for a winning strategy")))))

(defun render-rule-editor (game)
  (<ucw:form :action (setf (dialogue-rules game)
			     (call 'rule-editor
				   :game game))
    (<:p "You are welcome to change the game's ruleset.  The ruleset
that is currently in force can be found above, in the layout of the
game so far.")
    (<:p "If you proceed to edit the ruleset, you will be able to choose
from a pre-compiled list of notable rulesets, or, if you like, you can
construct your own custom ruleset.  Keep in mind that altering the
ruleset could very well render the current game incoherent (that is,
at least one of the game's moves violates at least one rule in the
ruleset).  If the game becomes incoherent owing to your ruleset edits,
you will be able to see the problematic moves and continue editing the
ruleset.  Before continuing playing the game, you will need to ensure
that all the rules in your edited ruleset are satisfied.")
    (<:submit :value "Edit the ruleset")))

(defun render-quit-form ()
  (<:p "Quitting the game will discard whatever progress you've made so far and return you to the initial page.")
  (<ucw:form :method "post"
	     :action (call 'start-game-component)
	  (<:submit :value "Quit")))

(defmethod render ((self turn-editor))
    (let* ((game (game self))
	   (game-len (dialogue-length game))
	   (play-style (play-style self)))
      (<:h1 "The game so far")
      (<:div :style "border:1px solid"
        (render-game game 
		     :play-style play-style
		     :extra-rules (extra-rules self)
		     :heuristics (heuristics self)
		     :indicate-alternatives t))
      (<:h1 "Choose from the available moves...")
      (ecase (play-style self)
	(play-as-both-proponent-and-opponent
	 (render-available-moves game))
	(play-as-proponent-random-opponent
	 (render-available-proponent-moves game))
	(play-as-opponent-random-proponent
	 (render-available-opponent-moves game)))
      (<:h1 "...or search for a win...")
      (render-win-searcher game play-style)
      (<:h1 "...or enter your move manually...")
      (render-manual-move-entry-form game play-style)
      (when (> game-len 1)
      	(<:h1 "...or rewind the game...")
      	(render-rewind-form game play-style))
      (<:h1 "...or edit the dialogue rules...")
      (render-rule-editor game)
      (<:h1 "...or quit.")
      (render-quit-form)))

(defcomponent alternative-move-chooser (game-component play-style-component ruleset-component)
  ((move-number :initarg :move-number
		:accessor move-number)
   (actual-play :initarg :actual-play
		:accessor actual-play)))

(defmethod render ((self alternative-move-chooser))
  (let ((game (game self))
	(play-style (play-style self))
  	(move-number (move-number self)))
    (<:h1 "Alternatives were available")
    (<:p "The game so far is:")
    (<:div :style "border:1px solid;"
      (render-game game
		   :play-style play-style
		   :extra-rules (extra-rules self)
		   :heuristics (heuristics self)
		   :moves-to-highlight (list move-number)))
    (<:p "Alternatives at move " (<:as-html move-number) ":")
    (<:ul
     (dolist (play (all-next-moves-at-position game move-number))
       (unless (equal-moves? play (actual-play self))
  	 (let ((player (move-player play))
  	       (statement (move-statement play))
  	       (stance (move-stance play))
  	       (reference (move-reference play)))
  	   (<:li (<ucw:a :action
  			(call 'turn-editor
  			      :game (add-move-to-dialogue-at-position
				     (truncate-dialogue
				      (copy-dialogue game)
				      move-number)
				     play
				     move-number)
			      :heuristics (heuristics self)
			      :extra-rules (extra-rules self)
			      :play-style play-style)
			(<:as-html
			 (if (eq player 'p)
			     "Proponent"
			     "Opponent"))
  			" could have "
  			(<:as-html
			 (if (eq stance 'a)
			     "attacked move "
			     "defended against the attack of move "))
  			(<:as-html reference)
  			" by asserting "
  			(render statement)))))))
    (<:p "Follow a link to rewind the current game to move " (<:as-html move-number) " and play the selected alternative move rather than what was actually asserted.  Or, " (<ucw:a :action
  					  (call 'turn-editor
						:play-style play-style
						:heuristics (heuristics self)
						:extra-rules (extra-rules self)
  						:game game)
  					  "continue playing the original game")
    ".")))

(defvar closed-attack-color "CCCCCC")
(defvar open-attack-color "CCCCFF")
(defvar alternative-attack-color "CC3300")

(defun render-move-in-game (play game move-number
			      &key indicate-alternatives
			           play-style
			           heuristics
			           extra-rules
			           attack-is-closed
			           move-is-alternative)
  (with-slots (player statement stance reference)
      play
    (let ((background-style (if (attacking-move? play)
				(format nil "background-color:#~A;"
					(if attack-is-closed
					    closed-attack-color
					    open-attack-color))
				"")))
    (<:tr :style background-style
      (<:td :align "left" (<:as-html move-number))
      (<:td :align "center" (<:as-html player))
      (if move-is-alternative
	  (let ((cell-style (concatenate 'string background-style
					 (format nil "border:3px solid #~A;"
						 alternative-attack-color))))
	    (<:td :style cell-style
		  :align "left"
		  (render statement)))
	  (<:td :align "left" (render statement)))
      (if (zerop move-number)
	  (<:td)
	  (<:td :align "right"
		(<:as-html "[" (if (attacking-move? play)
				   "A"
				   "D")
			   ","
			   reference
			   "]")))
      (if indicate-alternatives
	  (let ((all-previous-moves (all-next-moves-at-position game
								move-number)))
	    (if (null (cdr all-previous-moves)) ;; PLAY was the only one
		(<:td)
		(<:td :align "center"
		 (<ucw:a :action
			 (call 'alternative-move-chooser
			       :heuristics heuristics
			       :extra-rules extra-rules
			       :play-style play-style
			       :game game
			       :actual-play play
			       :move-number move-number)
			 :title "There were alternatives to this move"
			 (<:as-is "&#8224;")))))
	  (<:td))))))

(defun render-game (game &key indicate-alternatives
		              play-style
		              heuristics
		              extra-rules
		              moves-to-highlight)
  ;; (render-signature (dialogue-signature game))
  (let ((ruleset (dialogue-rules game)))
    (<:p (<:em "Base ruleset: ")
	 (progn
	   (<:strong (<:as-html (name ruleset)))
	   ": "
	   (<:as-html (description ruleset))))
    (<:p (<:em "Extra rules:")
	 (if extra-rules
	     (<:ul
	      (dolist (rule extra-rules)
		(<:li
		 (<:strong (<:as-html (name rule)))
		 ": "
		 (<:as-html (description rule)))))
	     (progn
	       (<:as-html " ")
	       (<:em "(none)"))))
    (<:p (<:em "Heuristic rules: ")
	 (render-heuristics heuristics)))
  (<:hr)
  (<:table 
   (<:colgroup :span "2" :align "center")
   (<:colgroup :span "3" :align "left")
   (<:thead
    (<:th "Move")
    (<:th "Player")
    (<:th "Assertion")
    (<:th "Stance, Reference")
    (<:th "Notes"))
   (<:tbody
    (let ((game-len (dialogue-length game)))
      (if (zerop game-len)
	  (<:tr
	   (<:td :colspan "5"
		 (<:em "(there are no moves in the game)")))
	  (let ((open-attacks (open-attack-indices game)))
	    (loop with plays = (dialogue-plays game)
	       with len = (length plays)
	       for play in plays
	       for i from 0 upto len
	       do
		 (render-move-in-game 
		  play game i
		  :indicate-alternatives indicate-alternatives
		  :heuristics heuristics
		  :extra-rules extra-rules
		  :play-style play-style
		  :attack-is-closed (not (member i open-attacks :test #'=))
		  :move-is-alternative (member i moves-to-highlight :test #'=)))))))
   (<:tfoot
    (<:tr 
     (<:td :align "center"
	   :colspan "5"
	   (<:em "Rows in " (<:span :style "background-color:#CCCCCC"
				    "grey")
		 " are closed attacks; rows in "
		 (<:span :style "background-color:#CCCCFF"
			 "blue")
		 " are open attacks.  (Defensive moves are not colored, nor is the initial move, since it is neither an attack nor a defense.) A dagger " (<:as-is "(&#8224;)") " in the Notes column indicates that alternative moves were available; follow the link to see them and rewind the game to explore an alternative course."))))))

(defmethod render ((statement term))
  (let ((func-sym (function-symbol statement))
	(args (arguments statement)))
    (<:em func-sym)
    (<:as-is "(")
    (if (null args)
	(<:as-is ")")
	(let ((first (car args)))
	  (render first)
	  (when (not (null (cdr args)))
	    (dolist (arg args)
	      (<:as-is ",")
	      (render arg)))
	  (<:as-is ")")))))

(defmethod render :around ((formula unary-connective-formula))
  (call-next-method)
  (render (argument formula)))

(defmethod render ((neg negation))
  (<:as-is "&not;"))

(defmethod render :around ((formula binary-connective-formula))
  (<:as-html "(")
  (render (lhs formula))
  (<:as-html " ")
  (call-next-method)
  (<:as-html " ")
  (render (rhs formula))
  (<:as-html ")"))

(defmethod render :around ((gen generalization))
  (call-next-method)
  (<:em (<:as-html (bound-variable gen)))
  (<:as-html "[")
  (render (matrix gen))
  (<:as-html "]"))

(defmethod render ((formula binary-conjunction))
  (<:as-is "&and;"))

(defmethod render ((formula binary-disjunction))
  (<:as-is "&or;"))

(defmethod render ((formula implication))
  (<:as-is "&rarr;"))

(defmethod render ((formula equivalence))
   (<:as-is "&harr;"))

(defmethod render ((formula universal-generalization))
  (<:as-is "&forall;"))

(defmethod render ((formula existential-generalization))
  (<:as-is "&exist;"))

(defmethod render ((formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (<:em (<:format "~(~a~)" pred))
    (unless (null args)
      (<:as-is "(")
      (let ((first (car args)))
	(render first)
	(when (not (null (cdr args)))
	  (dolist (arg args)
	    (<:as-is ",")
	    (render arg)))
	(<:as-is ")")))))

(defcomponent start-game-component (signature-component ruleset-component)
  ())

(defcomponent ruleset-info ()
  ())

(defmethod render ((self ruleset-info))
  (<:p (<:em (<:b "About the rules:")) " The rulesets in the
above menu are some notable cases that have some logical content.  You will be able to change your choice of ruleset once the game has started.  The names " (html-quote "D") " and " (html-quote "E") " come from W. Felscher's paper " (<:em "Dialogues, strategies, and intuitionistic provability") ", Annals of Pure and Applied Logic " (<:b "28") "(3), pp. 217" (<:as-is "&ndash;") "254, May 1985; it was arguably the first papers to rigorously establish the equivalence between intuitionistic validity and existence of winning strategies for certain dialogue games.  You will be able to alter your choice of rules after the game has begun."))

(defmethod render ((self start-game-component))
  (let (input-formula
	selected-formula 
	selected-rules
	selected-translation
	selected-play-style
	d-dialogue-rules-selected
	e-dialogue-rules-selected
	classical-dialogue-rules-selected
	nearly-classical-dialogue-rules-selected
	rule-d10-checked
	rule-d11-checked
	rule-d12-checked
	rule-d13-checked
	rule-e-checked
	rule-d10-literal-checked
	rule-d11-most-recent-attack-checked
	rule-d11-queue-checked
	rule-d11-proponent-checked
	rule-d11-opponent-checked
	rule-d13-symmetric-checked
	rule-d12-two-times-checked
	rule-d13-two-times-checked
	rule-d13-three-times-checked
	rule-d14-checked
	proponent-no-repeats-checked
	opponent-no-repeats-checked
	rule-no-repetitions-checked)
  (symbol-macrolet
      (($formula
	(let ((sig (signature self)))
	  (if (eq selected-formula t)
	      (call 'manual-formula-editor-component
		    :signature sig)
	      (if (belongs-to-signature? sig selected-formula)
		  selected-formula
		  (call 'formula-corrector
			:text selected-formula
			:signature sig)))))
       ($extra-rules
	(let (extras)
	  (when rule-d10-checked
	    (push rule-d10 extras))
	  (when rule-d11-checked
	    (push rule-d11 extras))
	  (when rule-d12-checked
	    (push rule-d12 extras))
	  (when rule-d13-checked
	    (push rule-d13 extras))
	  (when rule-e-checked
	    (push rule-e extras))
	  (when rule-d10-literal-checked
	    (push d10-literal extras))
	  (when rule-d11-most-recent-attack-checked
	    (push d11-most-recent-attack extras))
	  (when rule-d11-queue-checked
	    (push d11-queue extras))
	  (when rule-d11-proponent-checked
	    (push rule-d11-proponent extras))
	  (when rule-d11-opponent-checked
	    (push rule-d11-opponent extras))
	  (when rule-d13-symmetric-checked
	    (push rule-d13-symmetric extras))
	  (when rule-d12-two-times-checked
	    (push rule-d12-two-times extras))
	  (when rule-d13-two-times-checked
	    (push rule-d13-two-times extras))
	  (when rule-d13-three-times-checked
	    (push rule-d13-three-times extras))
	  (when rule-d14-checked
	    (push rule-d14 extras))
	  (reverse extras)))
       ($heuristics
	(let (heuristics)
	  (when rule-no-repetitions-checked
	    (push rule-no-repetitions heuristics))
	  (when opponent-no-repeats-checked
	    (push opponent-no-repeats heuristics))
	  (when proponent-no-repeats-checked
	    (push proponent-no-repeats heuristics))
	  (reverse heuristics)))
       ($ruleset
	(or (ruleset self)
	    (copy-ruleset
	     (ecase selected-rules
	       (d-dialogue-rules d-dialogue-rules)
	       (e-dialogue-rules e-dialogue-rules)
	       (classical-dialogue-rules classical-dialogue-rules)
	       (nearly-classical-dialogue-rules nearly-classical-dialogue-rules)
	       (skeletal-rules skeletal-rules)))))
       ($trimmed-extra-rules
	(remove-if #'(lambda (rule)
		       (member rule (rules $ruleset)))
		   $extra-rules))
       ($actual-ruleset
	(make-instance 'ruleset
		       :name (name $ruleset)
		       :description (description $ruleset)
		       :rules (remove-duplicates
			       (append (rules $ruleset)
				       $extra-rules
				       $heuristics))))
       ($take-action
	(ecase selected-play-style
	  (interactive-strategy-search-for-proponent
	   (let* ((initial-move (make-move 'p
					   (apply-translation selected-translation $formula)
					   nil
					   nil))
		  (root (make-instance 'strategy-node :move initial-move))
		  (strat (make-instance 'strategy
					:ruleset $actual-ruleset
					:root root)))
	     (call 'strategy-editor
		   :strategy strat
		   :player 'p
		   :extra-rules $trimmed-extra-rules
		   :heuristics $heuristics)))
	  (interactive-strategy-search-for-opponent
	   (let* ((initial-move (make-move 'p
					   (apply-translation selected-translation $formula)
					   nil
					   nil))
		  (root (make-instance 'strategy-node :move initial-move))
		  (strat (make-instance 'strategy
					:ruleset $actual-ruleset
					:root root)))
	     (call 'strategy-editor
		   :strategy strat
		   :player 'o
		   :extra-rules $trimmed-extra-rules
		   :heuristics $heuristics)))
	  (play-as-both-proponent-and-opponent
	   (call 'turn-editor
		 :play-style 'play-as-both-proponent-and-opponent
		 :extra-rules $trimmed-extra-rules
		 :heuristics $heuristics
		 :game (make-dialogue (apply-translation selected-translation $formula)
				      sig
				      $actual-ruleset)))
	  (play-as-proponent-random-opponent
	   (call 'turn-editor
		 :play-style 'play-as-proponent-random-opponent
		 :extra-rules $trimmed-extra-rules
		 :heuristics $heurstics
		 :game (let ((initial-dialogue
			      (make-dialogue (apply-translation selected-translation $formula)
					     sig
					     $actual-ruleset)))
			 (let* ((next-opponent-attacks (next-attacks initial-dialogue 'o))
				(next-opponent-defenses (next-defenses initial-dialogue 'o))
				(all-opponent-moves (append next-opponent-attacks
							    next-opponent-defenses)))
			   (if (null all-opponent-moves)
			       (call 'turn-editor
				     :play-style 'play-as-proponent-random-opponent
				     :extra-rules $trimmed-extra-rules
				     :heuristics $heuristics
				     :game initial-dialog)
			       (let ((random-move (random-element all-opponent-moves)))
				 (destructuring-bind (statement reference)
				     random-move
				   (if (member random-move next-opponent-attacks)
				       (call 'turn-editor
				 
					     :play-style 'play-as-proponent-random-opponent
					     :extra-rules $trimmed-extra-rules
					     :heuristics $heuristics
					     :game (add-move-to-dialogue-at-position initial-dialogue
										     (make-move 'o statement 'a reference)
										     1))
				       (call 'turn-editor
					     :play-style 'play-as-proponent-random-opponent
					     :extra-rules $trimmed-extra-rules
					     :heuristics $heuristics
					     :game (add-move-to-dialogue-at-position initial-dialogue
										     (make-move 'o statement 'd reference)
										     1))))))))))
	  (play-as-opponent-random-proponent
	   (call 'turn-editor
		 :play-style 'play-as-opponent-random-proponent
		 :extra-rules $trimmed-extra-rules
		 :heuristics $heuristics
		 :game (make-dialogue (apply-translation selected-translation $formula)
				      sig
				      $actual-ruleset))))))
    (let ((sig (signature self)))
      (<ucw:form
       :method "post"
       :action $take-action
       (<:table
	:style "border:1px solid;"
	(<:caption 
	 :style "caption-side:bottom;"
	 (<:submit
	  :title "Start playing a game with the selected formula and ruleset."
	  :value "Let's play"))
	(<:tbody
	 :style "border:1px solid;"
	 (<:tr 
	  :style "background-color:#F063CD;"
	  (<:td (<ucw:a 
		 :action (call 'formula-info)
		 "Formula:"))
	  (<:td
	   (<ucw:select
	    :id "selected-formula" 
	    :size 1 
	    :accessor selected-formula
	    (dolist (famous-formula (cons 't famous-formulas))
	      (if (eq famous-formula t)
		  (<ucw:option
		   :value t
		   (<:as-is "(enter a formula manually)"))
		  (destructuring-bind (long-name short-name formula)
		      famous-formula
		    (declare (ignore short-name))
		    (<ucw:option
		     :value formula (<:as-is long-name))))))))
	 (<:tr
	  :style "background-color:#A7007D;"
	  (<:td (<ucw:a 
		 :action (call 'translation-info)
		 "Translation:"))
	  (<:td (<ucw:select
		 :id "selected-translation"
		 :size 1
		 :accessor selected-translation
		 (dolist (translation available-translations)
		   (<ucw:option
		    :value translation
		    (<:as-is (description translation)))))))
	 (<:tr
	  :style "background-color:#7B942E;"
	  (<:td :title "To construct a ruleset for playing a dialogue game, first, choose a base ruleset.  The list of base rulesets is taken from the literature on dialogue games (see, for instance, W. Felscher's 'Dialogues, strategies, and intuitionistic provability', Annals of Pure and Applied Logic 28(3), pp. 217-254).  (The names 'D' and 'E' and the names of the standard structural rules come from this paper.)

After choosing a base ruleset, you may optionally select other rules. The extra rules come in three kinds: standard structural rules, experimental, non-standard structural rules, and heuristics.  Strictly speaking, there is no difference between these different kinds of rules; they are all structural rules on a par with one another.

The standard structural rules are commonly used in the literature on dialogue games (Felscher, Krabbe, etc.).

The experimental rules are just that: experiments.  They are definitely non-standard, and sometimes even whimsical.

The list of heuristic rules can be used to help cut down the set of possibilities.  For example, one might be interested in games where neither player can repeat and earlier move.  (But note that these heuristic rules, since they are on a par with all the other rules, can have significant logical impact.  You have been warned.)

The ruleset that will be used during the game will be the union of the rules in the chosen base rule set, together with whatever optional, extra rules were chosen.  We have a adopted a 'the-user-is-always-right' approach: there is no check for whether the constructed ruleset is 'consistent' or has any logical significance."
		"Ruleset")
	  (<:td (if (null (ruleset self))
		    (<:table
		     :rules "cols"
		     :summary "The purpose of this table is to build the ruleset according to which you want to play a game."
		     (<:thead
		      (<:colgroup
		       (<:col)
		       (<:col)
		       (<:col))
		      (<:tr
		       (<:th
			:title "First, choose from one of the predefined well-established rulesets.  Its rules will be included in the ruleset according to which your game will be played.  By default, only a skeletal ruleset (no structural rules) will be used."
			:abbr "Ruleset"
			"Standard Rulesets")
		       (<:th
			:title "Now choose whether to include some of the standard rules taken from the literature on dialogue games.  Some of the standard rulesets already include these rules; if you choose a rule here that is already included in the ruleset you've chosen, you choice will be ignored."
			:abbr "Structural Rules"
			"Standard Structural Rules")
		       (<:th
			:title "Now choose whether to include some non-standard experimental structural rules."
			:abbr "Experimental"
			"Experimental Rules")
		       (<:th
			:title "Finally, choose whether you with to include some 'heuristic' rules that will help to eliminate some 'redundant' possibilities (e.g., repeating a move)."
			:abbr "Heuristics"
			"Heuristic Rules")))
		     (<:tbody
		      (<:tr
		       :valign "middle"
		       (<:td
			(<ucw:select
			 :accessor selected-rules
			 :title "Choose from one of the predefined well-established rulesets.  Its rules will be included in the ruleset according to which your game will be played.  By default, only a skeletal ruleset (no structural rules) will be used."
			 (ruleset-option skeletal-rules)
			 (ruleset-option d-dialogue-rules)
			 (ruleset-option e-dialogue-rules)
			 (ruleset-option classical-dialogue-rules)
			 (ruleset-option nearly-classical-dialogue-rules)))
		       (<:td
			(<:table
			 :rules "rows"
			 (rule-checkbox-row rule-d10 rule-d10-checked)
			 (rule-checkbox-row rule-d11 rule-d11-checked)
			 (rule-checkbox-row rule-d12 rule-d12-checked)
			 (rule-checkbox-row rule-d13 rule-d13-checked)	 
			 (rule-checkbox-row rule-e rule-e-checked)))
		       (<:td
			(<:table
			 :rules "rows"
			 (rule-checkbox-row rule-d10-literal rule-d10-literal-checked)
			 (rule-checkbox-row rule-d11-most-recent-attack rule-d11-most-recent-attack-checked)
			 (rule-checkbox-row rule-d11-queue rule-d11-queue-checked)
			 (rule-checkbox-row rule-d11-proponent rule-d11-proponent-checked)
			 (rule-checkbox-row rule-d11-opponent rule-d11-opponent-checked)
			 (rule-checkbox-row rule-d13-symmetric rule-d13-symmetric-checked)
			 (rule-checkbox-row rule-d12-two-times rule-d12-two-times-checked)
			 (rule-checkbox-row rule-d13-two-times rule-d13-two-times-checked)
			 (rule-checkbox-row rule-d13-three-times rule-d13-three-times-checked)
			 (rule-checkbox-row rule-d14 rule-d14-checked)))
		       (<:td
			(<:table
			 :rules "rows"
			 (rule-checkbox-row rule-no-repetitions rule-no-repetitions-checked)
			 (rule-checkbox-row opponent-no-repeats opponent-no-repeats-checked)
			 (rule-checkbox-row proponent-no-repeats proponent-no-repeats-checked))))))
		    (<:as-html (description (ruleset self))))))
	 (<:tr
	  :style "background-color:#A3D800;"
	  (<:td (<ucw:a
		 :action (call 'play-style-info)
		 "Play style:"))
	  (<:td (<ucw:select
		 :id "selected-play-style"
		 :size 1
		 :accessor selected-play-style
		 (<ucw:option
		  :value 'play-as-both-proponent-and-opponent
		  "Play a game as both Proponent and Opponent")
		 (<ucw:option
		  :value 'play-as-proponent-random-opponent
		  "Play a game as Proponent (Opponent will choose its moves randomly)")
		 (<ucw:option
		  :value 'play-as-opponent-random-proponent
		  "Play a game as Opponent (Proponent will choose its moves randomly)")
		 (<ucw:option
		  :value 'interactive-strategy-search-for-proponent
		  "Search for a winning strategy for Proponent")
		 (<ucw:option
		  :value 'interactive-strategy-search-for-opponent
		  "Search for a winning strategy for Opponent")))))))))))

;;; ucw-site.lisp ends here