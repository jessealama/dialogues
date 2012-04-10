;;; ucw-turns.lisp UCW functionality for taking turns in dialogues

(in-package :dialogues)

(defclass game-component ()
  ((game :accessor game
	 :initarg :game
	 :initform nil
	 :type (or (eql nil) dialogue))))

(defclass play-style-component ()
  ((play-style :accessor play-style
	       :initarg :play-style
	       :initform nil)))

(defcomponent turn-editor (game-component play-style-component ruleset-component)
  ())

(defcomponent rule-editor (game-component ruleset-component play-style-component)
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
			    (<:td :align "left" (<:as-is (render-fancily statement)))
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
      (<:form
       :method "get"
       :action "/dialogues/"
       (<:submit :value "Quit"))))

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
  			(<:as-is (render-fancily statement))))))))
    (<:p "Follow a link to rewind the current game to move " (<:as-html move-number) " and play the selected alternative move rather than what was actually asserted.  Or, " (<ucw:a :action
  					  (call 'turn-editor
						:play-style play-style
						:heuristics (heuristics self)
						:extra-rules (extra-rules self)
  						:game game)
  					  "continue playing the original game")
    ".")))

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

(defparameter *closed-attack-color* "CCCCCC")
(defparameter *open-attack-color* "CCCCFF")
(defparameter *alternative-attack-color* "CC3300")

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
					    *closed-attack-color*
					    *open-attack-color*))
				"")))
    (<:tr :style background-style
      (<:td :align "left" (<:as-html move-number))
      (<:td :align "center" (<:as-html player))
      (if move-is-alternative
	  (let ((cell-style (format nil "~a;border:3px solid #~A;"
				    background-style
				    *alternative-attack-color*)))
	    (<:td :style cell-style
		  :align "left"
		  (<:as-is (render-fancily statement))))
	  (<:td :align "left" (<:as-is (render-fancily statement))))
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
	      (dolist (ruleset *available-rulesets*)
		(<ucw:option :value ruleset
			     (<:as-html (description ruleset)))))
	   (<:submit :value "Use this ruleset instead of the current one"))))))

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
       "Attack move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement))))))

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
       "Defend against the attack of move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement))))))

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
		    "Attack move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)) " (you would win the game)")
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
		    "Attack move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)))))))

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
		    "Attack move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)) " (you would win the game)")
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
		    "Attack move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)))))))

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
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)) " (you would win the game)")
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
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)))))))

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
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)) " (you would win the game)")
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
		    "Defend against the attack of move " (<:as-html reference) " by asserting " (<:as-is (render-fancily statement)))))))

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
       (<:form
	:method "get"
	:action "/dialogues/"
	(<:submit :value "Quit"))))))

;;; ucw-turns.lisp ends here