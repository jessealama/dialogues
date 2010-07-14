;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogues)

(defvar *maintainer-email* "jesse.alama@gmail.com")

(defparameter available-rulesets
  (list d-dialogue-rules 
	e-dialogue-rules
	nearly-classical-dialogue-rules
	classical-dialogue-rules
	d-dialogue-rules-minus-d10
	d-dialogue-rules-minus-d11
	d-dialogue-rules-minus-d12
	d-dialogue-rules-symmetric-d13
	d-dialogue-rules-literal-d10))

(defparameter available-translations
  (list identity-translation
	gödel-gentzen-translation 
	double-negate-translation
	double-negate-all-subformulas-translation
	kuroda-translation
	negate-atomic-subformulas-translation
	double-negate-atomic-subformulas-translation
	self-conjoin-atomic-subformulas-translation
	self-disjoin-atomic-subformulas-translation
	contrapositivify-translation
	atomic->excluded-middle-translation))

(defclass ruleset-component ()
  ((ruleset :initarg :ruleset
	    :initform nil
	    :accessor ruleset
	    :type (or (eql nil) ruleset))))

(defclass signature-component ()
  ((signature :initarg :signature
	      :initform pqrs-propositional-signature
	      :type finite-variable-propositional-signature
	      :accessor signature)))

(defcomponent signature-editor (signature-component)
  ())

(defaction save-signature ((self signature-editor) new-predicate-symbol)
  (add-predicate (signature self) 
		 (intern-in-dialogue-package new-predicate-symbol)
		 0)
  (answer (signature self)))

(defclass game-component ()
  ((game :accessor game
	 :initarg :game
	 :initform nil
	 :type (or (eql nil) dialogue))))

(defclass play-style-component ()
  ((play-style :accessor play-style
	       :initarg :play-style)))

(defmethod render ((self signature-editor))
  (with-slots ((sig signature))
      self
    (<:p "The signature that will be used during the game is:")
    (render-signature sig)
    (<:p
     "[" (<ucw:a :action (call 'add-a-predicate
			       :signature sig
			       :name nil)
		 "add a predicate") 
     "]"
     " "
     "["
     (<ucw:a :action (call 'delete-a-predicate :signature sig)
	     "delete a predicate")
     "]"
     " "
     "["
     (<ucw:a :action (answer sig) "proceed")
     "]")))

;; adding a predicate

(defcomponent add-a-predicate (signature-component)
  ((proposed-name :initarg :name 
		  :accessor proposed-name
		  :initform nil)
   (validation-error-message :initarg :error-message
			     :accessor validation-error-message
			     :initform nil)))

(defmethod handle-toplevel-condition ((app (eql *dialogue-application*))
				      condition
				      action)
  (<:h1 "Oops, something went wrong")
  (<:p "Something went wrong, I'm afraid.  Here is the precise error that was generated:")
  (<:blockquote
   (<:as-html condition))
  (<:p "You're welcome to " (<:a :href (concatenate 'string
						    "mailto:"
						    *maintainer-email*)
				 "notify the site maintainer") " about this.")
  (<:p "What next?  You can either go back with your browser or simply " 
       (<ucw:a :action
	       (let* ((default-fec (make-instance 'formula-entry-component 
						  :signature (copy-signature pqrs-propositional-signature)))
		      (default-sgc (make-instance 'start-game-component 
						  :formula-entry-component default-fec)))
		 (call 'initial-formula-window :body default-sgc))
	       "quit and start over") "."))

(defaction insert-predicate (signature pred-name)
  (ucw-handler-case (add-predicate signature pred-name 0)
    (unacceptable-identifier-name-error (c)
      (let ((text (unacceptable-identifier-name-error-text c)))
	(answer
	 (call 'add-a-predicate
	       :signature signature
	       :error-message (format nil "The predicate name that you gave previously, \"~A\", contains a whitespace character and is thus unacceptable." text)
	       :name text))))
    (symbol-already-present-error (c)
      (let ((text (symbol-already-present-error-symbol c)))
	(answer
	 (call 'add-a-predicate
	       :signature signature
	       :error-message (format nil "The predicate name that you gave previously, \"~A\", already belongs to the signature." text)
	       :name text))))
    (:no-error (result)
	       (answer result))))

(defmethod render ((self add-a-predicate))
  (let (input-predicate-name)
    (with-slots ((new-name proposed-name) 
		 (sig signature) 
		 (message validation-error-message))
	self
      (when message
	(<:div :class "error-message"
	  (<:p (<:as-html message) " " "Please try again.")))
      (<:p "The current signature is:")
      (render-signature sig)
      (<:p "The new predicate name should be different from the names of currently existing predicates.  It should be different from the empty string and should not contain any whitespace characters.")
      (<ucw:form :method "POST"
		 :action (answer (insert-predicate sig input-predicate-name))
        (<:label :for "new-predicate-name" "New predicate name")
	(<ucw:input :type "text" 
		    :id "new-predicate-name"
		    :accessor input-predicate-name)
	(<:submit :value "Add this predicate")))))

;; deleting a predicate

(defcomponent delete-a-predicate ()
  ())

(defmethod render ((self delete-a-predicate))
  (let (selected-predicate)
    (symbol-macrolet 
	(($take-action (answer (delete-predicate (signature self) selected-predicate))))
      (with-slots ((sig signature))
	  self
	(<:h1 "Deleting a predicate")
	(if (signature-predicates sig)
	    (<ucw:form :method "POST"
		       :action $take-action
	      (<:p "Choose a predicate to be deleted from the signature:")
	      (<ucw:select :size 1
			   :accessor selected-predicate
	        (dolist (pred (signature-predicates sig))
		  (<ucw:option :value pred
			       (<:as-html pred))))
	      (<ucw:submit :value "Delete this predicate"
			   :action $take-action))
	    (<:p "There are no predicates in the signature; none can be deleted. " (<ucw:a :action (answer (signature self))
											  "Proceed") "."))))))

(defgeneric render-signature (signature))

(defmethod render-signature ((sig finite-variable-propositional-signature))
  (with-slots (predicates) sig
    (if (null predicates)
	(<:em "(empty signature)")
	(let ((first (car predicates)))
	  (<:em (<:as-html first))
	  (dolist (pred (cdr predicates))
	    (<:as-is ", ")
	    (<:em (<:as-html pred)))))))

(defentry-point "" (:application *dialogue-application*)
    ()
  (let* ((default-fec (make-instance 'formula-entry-component 
				     :signature (copy-signature
						 pqrs-propositional-signature)))
	 (default-sgc (make-instance 'start-game-component 
				     :formula-entry-component default-fec)))
    (call 'initial-formula-window :body default-sgc)))

(defcomponent initial-formula-window (standard-window-component)
  ()
  (:default-initargs
      :title "play a lorenzen dialogue game"))

(defparameter famous-formulas
  `(("Peirce's formula" "peirce-formula" ,peirce-formula)
    ("Excluded middle" "excluded-middle" ,excluded-middle)
    ("Weak excuded middle" "weak-excluded-middle" ,weak-excluded-middle)
    ("Dummett's formula" "dummett-formula" ,dummett-formula)
    ("Double negation introduction" "double-negation-intro" ,double-negation-intro)
    ("Double negation elimination" "double-negation-elim" ,double-negation-elimination)
    ("K formula" "k-formula" ,k-formula)
    ("B formula" "b-formula" ,b-formula)
    ("C formula" "c-formula" ,c-formula)
    ("W formula" "w-formulas" ,w-formula)
    ("Scott's formula" "scott-formula" ,scott-formula)
    ("Smetanich's formula" "smetanich-formula" ,smetanich-formula)
    ("De Morgan &not;(P &and; Q) &rarr; (&not;P &or; &not;Q)" "de-morgan-not-and-implies-or" ,de-morgan-not-and-implies-or)
    ("De Morgan &not;(P &or; Q) &rarr; (&not;P &and; &not;Q)" "de-morgan-not-or-implies-and" ,de-morgan-not-or-implies-and)
    ("De Morgan (&not;P &and; &not;Q) &rarr; &not;(P &or; Q)" "de-morgan-and-not-implies-not-or" ,de-morgan-and-not-implies-not-or)
    ("De Morgan (&not;P &or; &not;Q) &rarr; &not;(P &and; Q)" "de-morgan-or-not-implies-not-and" ,de-morgan-or-not-implies-not-and)
    ("(P &rarr; &not;P) &or; (&not;P &rarr; P)" "this-does-not-matter" ,anti-connexive)))

(defcomponent formula-corrector (signature-component)
  ((text :initarg :text :accessor formula-corrector-text)))

(defun formula-guide ()
  (<:p "Non-atomic formulas are written in prefix notation, with parentheses
      around the outside.  Thus, an implication whose antecdent is " (<:as-is "&phi;") " and whose consequent is " (<:as-is "&psi;") " would be entered as")
  (<:blockquote
   (<:pre "(implies " (<:as-is "&phi;") " " (<:as-is "&psi;") ")"))
  (<:p "The available connectives are")
  (<:ul
   (<:li (<:tt "implies") ",")
   (<:li (<:tt "iff") ",")
   (<:li (<:tt "and") ",")
   (<:li (<:tt "or") ", and")
   (<:li (<:tt "not") "."))
  (<:p "Atomic formulas are to be constructed according to the signature.  The case you use to write connectives and atomic formulas doesn't matter (anything you enter will be upcased)."))

(defaction parse-formula-action (formula-str signature)
  (ucw-handler-case
      (answer (parse-formula formula-str))
    (malformed-formula-error ()
      (answer (call 'formula-corrector
		    :text formula-str
		    :signature (signature self))))))

(defmethod render ((self formula-corrector))
  (let ((input-formula)
	(sig (signature self))
	(text (formula-corrector-text self)))
    (<:h1 "Invalid formula supplied")
    (<:p "We are unable to make sense of the formula, \""
	 (if (stringp text)
	     (<:as-html text)
	     (render text)) "\" that you supplied.  The signature with respect to which you should enter a formula is:")
    (render-signature sig)
    (formula-guide)
    (<:p "Please try again.")
    (<ucw:form :method "POST"
	       :action (parse-formula-action input-formula sig)
      (<:p "Enter a formula in the above signature.  If you wish, you can "
	   (<ucw:a :action 
		   (let ((new-signature (call 'signature-editor :signature sig)))
		     (if (formula? text)
			 (if (belongs-to-signature? new-signature text)
			     (answer text)
			     (answer
			      (call 'formula-corrector
				    :text text
				    :signature new-signature)))
			 (ucw-handler-case
			     (answer (parse-formula 
				      (formula-corrector-text self)))
			   (malformed-formula-error 
			    () 
			    (answer (call 'formula-corrector
					  :text input-formula
					  :signature new-signature))))))
		   "edit the signature") ". (If edit the signature and the formula that you provided becomes well-formed in the new signature, then you will go back to where you were before you came here.  If, after editing the signature, the formula is still not valid, you will come back to this page.)")
	(<ucw:input :type "text"
		    :id "formula-input"
		    :accessor input-formula)
	(<:submit :value "Use this formula"))))

(defcomponent turn-editor (game-component play-style-component)
  ())

(defcomponent turn-evaluator (game-component play-style-component)
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
  (with-slots (player statement stance reference game play-style)
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
			    :play-style play-style
			    :attack-is-closed (not (member i open-attacks 
							   :test #'=))))
		      (<:tr :style "background-color:#FF3333;"
			    (<:td :align "left" (<:as-html game-len))
			    (<:td :align "center" (<:as-html player))
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
				   " are open attacks.  (Defensive moves are not colored.)")))))))
		(<:p "At least one of the dialogue rules is violated by your proposed move:")
		(<:ul
		 (dolist (violated-rule violated-rules)
		   (<:li "Rule " (<:as-html (name violated-rule))
			 (<:br)
			 "Description: " (<:as-html (description violated-rule)))))
		(<ucw:form :method "POST"
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
will inadmissible and you will see which of the rules are violated by
your move.  Entering moves manully is a good way to explore the
meaning of the dialogue rules.")
      (<ucw:form :method "POST"
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
		          (<ucw:option :value attack-left-conjunct 
				       "Attack the left conjunct")
			  (<ucw:option :value attack-right-conjunct
				       "Attack the right conjunct")
			  (<ucw:option :value which-disjunct?
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
    (<ucw:form :method "POST"
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

(defun render-signature-editor (game)
  (let ((sig (dialogue-signature game)))
    (<:p "The signature being used in this game is:")
    (<:blockquote
     (render-signature sig))
    (<:p "You are welcome to edit the signature, if you wish.")
    (<:p "Bear in mind, though, that editing the signature will make
no substantive difference to the game unless you are working with
custom dialogue rules:")
    (<:ul
     (<:li "if you " (<:b "delete") " predicates from the signature,
some formulas occuring in the game that are well-formed now with
respect to the current signature may become malformed with respect to
the diminished signature, thus rendering the game incoherent.  Yet if
all the formulas occuring in the game remain well-formed with respect
to the diminished signature, the set of formulas that can be asserted
in the game from this point forward is unchanged, when working with
the usual dialogue rules (Felscher's D-rules or E-rules) because
formulas occuring in games that adhere to these rules are all
subformulas of the initial formula;")
     (<:li "if you " (<:b "add" ) " predicates to the signature, you
conceivably enlarge the set of assertions that can be asserted later
in the game, but this is not the case when using the usual dialogue
rules (such as Felscher's D-rules and E-rules); only in the presence
of custom dialogue rules can adding predicates to the signature now
make a difference."))
    (<:p "If you edit the signature, all the assertions made so far in
the game will be evaluated to ensure that they are well-formed
formulas in the modified signature. If any of the assertions cease to
be well-formed formulas, you will be required to further edit the
signature to ensure that they are well-formed.  If all assertions are
well-formed, you will be brought back to this page after editing the
signature.")
    (<:blockquote
     (<:em "(This functionality is not yet implemented.)"))
    (<ucw:submit 
     :value "Edit the signature"
     :action (let* ((new-sig (call 'signature-editor
				  :signature sig))
		    (malformed (remove-if-not #'(lambda (assertion)
						  (belongs-to-signature? new-sig
									 assertion))
					     (dialogue-assertions game))))
	       (if (null malformed)
		   (setf (signature game) new-sig)
		   (call 'signature-editor
			 :signature new-sig))))))

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
      (render-game game :moves-to-highlight (mapcar #'car indices-and-violated-rules))
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
  (<ucw:form :method "POST"
	     :action (let* ((default-fec (make-instance 'formula-entry-component :signature (copy-signature pqrs-propositional-signature)))
				(default-sgc (make-instance 'start-game-component :formula-entry-component default-fec)))
			   (call 'initial-formula-window :body default-sgc))
	  (<:submit :value "Quit")))
		 

(defmethod render ((self turn-editor))
    (let* ((game (game self))
	   (play-style (play-style self)))
      (<:h1 "The game so far")
      (<:div :style "border:1px solid"
        (render-game game 
		     :play-style play-style
		     :indicate-alternatives t))
      (<:h1 "Choose from the available moves...")
      (ecase (play-style self)
	(play-as-both-proponent-and-opponent
	 (render-available-moves game))
	(play-as-proponent-random-opponent
	 (render-available-proponent-moves game))
	(play-as-opponent-random-proponent
	 (render-available-opponent-moves game)))
      (<:h1 "...or enter your move manually...")
      (render-manual-move-entry-form game play-style)
      ;; (when (> game-len 1)
      ;; 	(<:h1 "...or rewind the game...")
      ;; 	(render-rewind-form game play-style))
      ;; (<:h1 "...or edit the signature...")
      ;; (render-signature-editor game)
      (<:h1 "...or edit the dialogue rules...")
      (render-rule-editor game)
      (<:h1 "...or quit.")
      (render-quit-form)))

(defcomponent alternative-move-chooser (game-component play-style-component)
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
    (render-game game
		 :play-style play-style
		 :moves-to-highlight (list move-number))
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
  						:game game)
  					  "continue playing the original game")
    ".")))

(defvar closed-attack-color "CCCCCC")
(defvar open-attack-color "CCCCFF")
(defvar alternative-attack-color "CC3300")

(defun render-move-in-game (play game move-number
			      &key indicate-alternatives
			           play-style
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
	  (<:td :align "right" (<:em "(initial move)"))
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
			       :play-style play-style
			       :game game
			       :actual-play play
			       :move-number move-number)
			 :title "There were alternatives to this move"
			 (<:as-is "&#8224;")))))
	  (<:td))))))

(defun render-game (game &key indicate-alternatives
		              play-style
		              moves-to-highlight)
  (render-signature (dialogue-signature game))
  (let ((ruleset (dialogue-rules game)))
    (<:p (<:em "Ruleset: ")
	 (<:as-html (description ruleset))))
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
		 " are open attacks.  (Defensive moves are not colored.) A dagger " (<:as-is "(&#8224;)") " in the Notes column indicates that alternative moves were available; follow the link to see them and rewind the game to explore an alternative course."))))))

(defgeneric render-plainly (statement))

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

(defmethod render-plainly ((statement term))
  (let ((func-sym (function-symbol statement))
	(args (arguments statement)))
    (if (null args)
	(format nil "~A" func-sym)
	(if (null (cdr args))
	    (format nil "~A(~A)"
		    func-sym
		    (render-plainly (car args)))
	    (funcall #'concat-strings
		     (format nil "~A" func-sym)
		     "("
		     (render-plainly (car args))
		     (apply #'concat-strings
			    (mapcar #'(lambda (arg)
					(format ",~A" (render-plainly arg)))
				    (cdr args)))
		     ")")))))

(defmethod render ((sa (eql attack-left-conjunct)))
  (<:as-is "&and;")
  (<:sub "L"))

(defmethod render-plainly ((sa (eql attack-left-conjunct)))
  "&and;(L)")

(defmethod render ((sa (eql attack-right-conjunct)))
  (<:as-is "&and;")
  (<:sub "R"))

(defmethod render-plainly ((sa (eql attack-right-conjunct)))
  "&and;(R)")

(defmethod render ((sa (eql which-instance?)))
  (<:as-is "?"))

(defmethod render-plainly ((sa (eql which-instance?)))
  "?")

(defmethod render ((sa (eql which-disjunct?)))
  (<:as-is "?"))

(defmethod render-plainly ((sa (eql which-disjunct?)))
  "?")	   

(defmethod render :around ((formula unary-connective-formula))
  (call-next-method)
  (render (argument formula)))

(defmethod render-plainly :around ((formula unary-connective-formula))
  (let ((body (call-next-method)))
    (concatenate 'string body (render-plainly (argument formula)))))

(defmethod render ((neg negation))
  (<:as-is "&not;"))

(defmethod render-plainly ((neg negation))
  "&not;")

(defmethod render :around ((formula binary-connective-formula))
  (<:as-html "(")
  (render (lhs formula))
  (<:as-html " ")
  (call-next-method)
  (<:as-html " ")
  (render (rhs formula))
  (<:as-html ")"))

(defmethod render-plainly :around ((formula binary-connective-formula))
  (concatenate 'string
	       "("
	       (render-plainly (lhs formula))
	       " "
	       (call-next-method)
	       " "
	       (render-plainly (rhs formula))
	       ")"))

(defmethod render :around ((gen generalization))
  (call-next-method)
  (<:em (<:as-html (bound-variable gen)))
  (<:as-html "[")
  (render (matrix gen))
  (<:as-html "]"))

(defmethod render-plainly :around ((gen generalization))
  (concatenate 'string
	       (call-next-method)
	       (render-plainly (bound-variable gen))
	       "["
	       (render-plainly (matrix gen))
	       "]"))

(defmethod render ((formula binary-conjunction))
  (<:as-is "&and;"))

(defmethod render-plainly ((formula binary-conjunction))
  "&and;")

(defmethod render ((formula binary-disjunction))
  (<:as-is "&or;"))

(defmethod render-plainly ((formula binary-disjunction))
  "&or;")

(defmethod render ((formula implication))
  (<:as-is "&rarr;"))

(defmethod render-plainly ((formula implication))
  "&rarr;")

(defmethod render ((formula equivalence))
   (<:as-is "&harr;"))

(defmethod render-plainly ((formula equivalence))
  "&harr;")

(defmethod render ((formula universal-generalization))
  (<:as-is "&forall;"))

(defmethod render-plainly ((formula universal-generalization))
  "&forall;")

(defmethod render ((formula existential-generalization))
  (<:as-is "&exist;"))

(defmethod render-plainly ((formula existential-generalization))
  "&exist;")

(defmethod render ((formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (<:em (<:as-html pred))
    (unless (null args)
      (<:as-is "(")
      (let ((first (car args)))
	(render first)
	(when (not (null (cdr args)))
	  (dolist (arg args)
	    (<:as-is ",")
	    (render arg)))
	(<:as-is ")")))))

(defmethod render-plainly ((formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (if (null args)
	(format nil "~A" pred)
	(if (null (cdr args))
	    (format nil "~A(~A)"
		    pred
		    (render-plainly (car args)))
	    (funcall #'concat-strings
		     (format nil "~A" pred)
		     "("
		     (render-plainly (car args))
		     (apply #'concatenate
			    'string
			    (mapcar #'(lambda (arg)
					(format ",~A" (render-plainly arg)))
				    (cdr args)))
		     ")")))))

(defcomponent start-game-component ()
  ((formula-entry-component :component t
			    :initarg :formula-entry-component
			    :accessor formula-entry-component)))

(defcomponent formula-entry-component (signature-component ruleset-component)
  ())

(defmethod render ((self formula-entry-component))
  (let (input-formula
	selected-formula 
	selected-rules
	selected-translation
	selected-play-style)
  (symbol-macrolet
      (($formula
	(let ((sig (signature self)))
	  (if (empty-string? input-formula)
	      (if (belongs-to-signature? sig selected-formula)
		  selected-formula
		  (call 'formula-corrector
			:text selected-formula
			:signature sig))
	      (let ((parsed-formula 
		     (ucw-handler-case (parse-formula input-formula)
		       (malformed-formula-error (call 'formula-corrector
						      :text input-formula
						      :signature sig)))))
		(if (belongs-to-signature? sig parsed-formula)
		    parsed-formula
		    (call 'formula-corrector
			  :text parsed-formula
			  :signature sig))))))
       ($take-action
	(ecase selected-play-style
	  (play-as-both-proponent-and-opponent
	   (call 'turn-editor
		 :play-style 'play-as-both-proponent-and-opponent
		 :game (make-dialogue (apply-translation selected-translation $formula)
				      sig
				      (if (null (ruleset self))
					  selected-rules
					  (ruleset self)))))
	  (play-as-proponent-random-opponent
	   (call 'turn-editor
		 :play-style 'play-as-proponent-random-opponent
		 :game (let ((initial-dialogue
			      (make-dialogue (apply-translation selected-translation $formula)
					     sig
					     (if (null (ruleset self))
						 selected-rules
						 (ruleset self)))))
			 (let* ((next-opponent-attacks (next-attacks initial-dialogue 'o))
				(next-opponent-defenses (next-defenses initial-dialogue 'o))
				(all-opponent-moves (append next-opponent-attacks
							    next-opponent-defenses)))
			   (if (null all-opponent-moves)
			       (call 'turn-editor
				     :play-style 'play-as-proponent-random-opponent
				     :game initial-dialog)
			       (let ((random-move (random-element all-opponent-moves)))
				 (destructuring-bind (statement reference)
				     random-move
				   (if (member random-move next-opponent-attacks)
				       (call 'turn-editor
					     :play-style 'play-as-proponent-random-opponent
					     :game (add-move-to-dialogue-at-position initial-dialogue
										     (make-move 'o statement 'a reference)
										     1))
				       (call 'turn-editor
					     :play-style 'play-as-proponent-random-opponent
					     :game (add-move-to-dialogue-at-position initial-dialogue
										     (make-move 'o statement 'd reference)
										     1))))))))))
	  (play-as-opponent-random-proponent
	   (call 'turn-editor
		 :play-style 'play-as-opponent-random-proponent
		 :game (make-dialogue (apply-translation selected-translation $formula)
				      sig
				      (if (null (ruleset self))
					  selected-rules
					  (ruleset self))))))))
    (let ((sig (signature self)))
      (<ucw:form :method "POST"
		 :action $take-action
      (<:table :style "border:1px solid;"
      (<:caption :style "caption-side:bottom;"
		 (<:submit :value "Let's play"))
       (<:tbody :style "border:1px solid;"
       (<:tr :style "background-color:#F0B2E0;"
	(<:td "The signature that will be used:")
	(<:td (render-signature sig)))
       (<:tr :style "background-color:#F063CD;"
	(<:td "Enter a formula or choose a famous formula from the menu:")
	(<:td
	 (<:table
	  (<:tr 
	   (<:td (<ucw:input :type "text" 
			     :accessor input-formula 
			     :id "input-formula")))
	  (<:tr
	   (<:td (<ucw:select :id "selected-formula" 
			      :size 1 
			      :accessor selected-formula
		   (dolist (famous-formula famous-formulas)
		     (destructuring-bind (long-name short-name formula)
			 famous-formula
		       (declare (ignore short-name))
		       (<ucw:option :value formula (<:as-is long-name))))))))))
       (<:tr :style "background-color:#A7007D;"
	(<:td "Select a translation to be applied to the selected formula:")
	(<:td (<ucw:select :id "selected-translation"
			   :size 1
			   :accessor selected-translation
	        (dolist (translation available-translations)
		  (<ucw:option :value translation
			       (<:as-html (description translation)))))))
       (<:tr :style "background-color:#7B942E;"
	(<:td "The ruleset to be used during the game:")
	(<:td (if (null (ruleset self))
		  (<ucw:select :id "selected-rules"
			       :size 1
			       :accessor selected-rules
			       (dolist (ruleset available-rulesets)
				 (<ucw:option :value ruleset
					      (<:as-html (description ruleset)))))
		  (<:as-html (description (ruleset self))))))
       (<:tr :style "background-color:#A3D800;"
         (<:td "Choose the style of play:")
	 (<:td (<ucw:select :id "selected-play-style"
			    :size 1
			    :accessor selected-play-style
	         (<ucw:option :value 'play-as-both-proponent-and-opponent
			      "Play as both proponent and opponent")
		 (<ucw:option :value 'play-as-proponent-random-opponent
			      "Play as Proponent (Opponent will choose its moves randomly)")
		 (<ucw:option :value 'play-as-opponent-random-proponent
			      "Play as Opponent (Propnent will choose its moves randomly)")))))
       (<:tfoot
	(<:tr
	 (<:td :colspan "2"
	       (<:em (<:b "About Lorenzen dialogue games:")) " Lorenzen
dialogues are a formalism for capturing intuitionistic validity using games.  Since their invention and development in the late 1950s and 1960s, they have been extended from intuitionistic first-order logic so that they apply to different notions of validity, such as those of classical logic, modal logics, linear logic, etc.  For more information, consult " (<:a :href "http://plato.stanford.edu/entries/logic-dialogical/" "the entry on dialogue games") " in the " (<:em "Stanford Encyclopedia of Philosophy")))
	(<:tr 
	  (<:td :colspan "2"
		(<:em (<:b "About the signature:")) " You can " (<ucw:a :action (call 'signature-editor :signature sig) "edit the signature") ", if you wish. (If you choose to edit the signature, you'll come back here when you're finished.)  You will not be able to edit the signature once the game begins."))
	(<:tr 
	  (<:td :colspan "2"
		(<:em (<:b "About the formula:")) " If the text box is not empty, its contents will be the initial formula of the game.  If the text box is empty, then the selected \"famous formula\" will be." 
		(formula-guide)))
	(<:tr 
	 (<:td :colspan "2"
	       (<:em (<:b "About the translation:")) " The default is the
identity translation, so that whatever formula is chosen (or whatever
formula is entered into the text box) will be, verbatim, the formula
with which the game begins."))
	(<:tr
	 (<:td :colspan "2"
	       (<:em (<:b "About the rules:")) " The rulesets in the
above menu are some notable cases that have some logical content.  You will be able to change your choice of ruleset once the game has started.  The names " (html-quote "D") " and " (html-quote "E") " come from W. Felscher's paper " (<:em "Dialogues, strategies, and intuitionistic provability") ", Annals of Pure and Applied Logic " (<:b "28") "(3), pp. 217" (<:as-is "&ndash;") "254, May 1985; it was arguably the first papers to rigorously establish the equivalence between intuitionistic validity and existence of winning strategies for certain dialogue games.  You will be able to alter your choice of rules after the game has begun."))
	(<:tr
	 (<:td :colspan "2"
	      (<:em (<:b "About the play style:")) " The default mode of playing is to take on the role of both players: at each move, you'll see all possible moves that can be made, from the perspective of both players.  Two other play styles are supported: play as Proponent with a random Opponent, and play as Opponent with a random Proponent.")))))))))

(defmethod render ((self start-game-component))
  (with-slots ((sig signature))
      self
    (render (formula-entry-component self))))

;;; ucw-site.lisp ends here