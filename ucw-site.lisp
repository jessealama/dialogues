;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogues)

(defvar *maintainer-email* "jesse.alama@gmail.com")

(defclass signature-component ()
  ((signature :initarg :signature
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
	 :type dialogue)))

(defmethod render ((self signature-editor))
  (with-slots ((sig signature))
      self
    (<:p "The signature that will be used during the game is:")
    (render sig)
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

(defcomponent add-a-predicate ()
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
      (render sig)
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

(defmethod render ((self finite-variable-propositional-signature))
  (with-slots (predicates) self
    (<:p "Predicates: "
      (if (null predicates)
	  (<:em "(none)")
	  (let ((first (car predicates)))
	    (<:em (<:as-html first))
	    (dolist (pred (cdr predicates))
	      (<:as-is ", ")
	      (<:em (<:as-html pred))))))))

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
    (render sig)
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

(defcomponent turn-editor (game-component)
  ((available-moves-list :component t
			 :accessor available-moves-list
			 :initform nil)))

(defcomponent available-moves-list (game-component)
  ())

(defcomponent turn-evaluator (game-component)
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
  (with-slots (player statement stance reference game)
      self
    (let ((game-len (dialogue-length game)))
    (if (and player statement stance reference)
	(multiple-value-bind (rules-result messages)
	    (evaluate-all-rules (dialogue-rules game)
				game 
				player 
				game-len
				statement 
				stance
				reference)
	  (if rules-result
	      (progn
		(<:h1 "Success!")
		(<:p "Your move was accepted. "
		     (<ucw:a :action 
			     (call 'turn-editor
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
		(render game)
		(<:p "Your proposed move:")
		(<:ul
		 (<:li "Player: " (<:as-html player))
		 (<:li "Statement: " (render statement))
		 (<:li "Stance: " (if (eq stance 'a)
				      "Attack"
				      "Defend"))
		 (<:li "Respond to statement number: " (<:as-html reference)))
		(<:p "At least one of the dialogue rules is violated by your proposed move:")
		(<:ul
		 (dolist (message messages)
		   (<:li (<:as-html message))))
		(<ucw:form :method "POST"
			   :action (call 'turn-editor :game game)
		  (<ucw:submit :value "Edit this move"
			       :action (call 'turn-editor :game game))))))))))

(defun render-attack (attack player game position)
  (destructuring-bind (statement reference)
      attack
    (<ucw:a
     :action (add-attack-to-dialogue-at-position game
						 player
						 statement
						 reference
						 position)
     "Attack move " (<:as-html reference) " by asserting " (render statement))))

(defun render-proponent-attack (attack game position)
  (render-attack attack 'p game position))

(defun render-opponent-attack (attack game position)
  (render-attack attack 'o game position))

(defun render-defense (defense player game position)
  (destructuring-bind (statement reference)
      defense
     (<ucw:a
      :action (add-defense-to-dialogue-at-position game
						   player
						   statement
						   reference
						   position)
	     "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement))))

(defun render-proponent-defense (defense game position)
  (render-defense defense 'p game position))

(defun render-opponent-defense (defense game position)
  (render-defense defense 'o game position))

(defun render-attacks (game)
  (let ((game-len (dialogue-length game)))
  (labels ((render-proponent-attacks-empty-opponent-attacks (attacks)
	     (dolist (attack attacks)
	       (<:tr
		(<:td (render-proponent-attack attack game game-len))
		(<:td))))
	   (render-opponent-attacks-empty-proponent-attacks (attacks)
	     (dolist (attack attacks)
	       (<:tr
		(<:td)
		(<:td (render-opponent-attack attack game game-len))))))
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
		      (render-proponent-attacks-empty-opponent-attacks (cdr last-man-tail)))
		    (render-proponent-attacks-empty-opponent-attacks last-man-tail)))
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
		      (render-opponent-attacks-empty-proponent-attacks (cdr last-man-tail)))
		    (render-opponent-attacks-empty-proponent-attacks last-man-tail)))))))))))

(defun render-defenses (game)
  (let ((game-len (dialogue-length game)))
  (labels
      ((render-proponent-defenses-empty-opponent-defenses (defenses)
	 (dolist (defense defenses)
	   (<:tr
	    (<:td (render-proponent-defense defense game game-len))
	    (<:td))))
       (render-opponent-defenses-empty-proponent-defenses (defenses)
	 (dolist (defense defenses)
	   (<:tr
	    (<:td)
	    (<:td (render-opponent-defense defense game game-len))))))
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
		      (render-proponent-defenses-empty-opponent-defenses (cdr last-man-tail)))
		    (render-proponent-defenses-empty-opponent-defenses last-man-tail)))
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
		      (render-opponent-defenses-empty-proponent-defenses (cdr last-man-tail)))
		    (render-opponent-defenses-empty-proponent-defenses last-man-tail)))))))))))
	    

(defun render-available-moves (game)
  (<:p "Below is a description of all available moves for both
Proponent and Opponent.")
  (render-attacks game)
  (<:br)
  (render-defenses game)
  (<:p (<:em "Note:") " The list of moves available to each player in
the following list is determined by the set of subformulas of all
formulas played so far. When using the usual rules for propositional
dialogue games (Felscher's D-rules and E-rules, for example), this is
sufficient: every assertion asserted in the course of a dialogue game
is a subformula of the initial formula, or is one of the symbolic
attacks (" (<:as-is "&and;<sub>L</sub>, &and;<sub>R</sub>, and
?)") ". If one modifies the rules, this structural fact about formulas
occuring in a dialogue game may no longer hold, so that the list below
may no longer be an exhaustive enumeration of all formulas that can be
asserted in the next move."))

(defun render-manual-move-entry-form (game)
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

(defun render-rewind-form (game)
  (let (rewind-point)
  (let ((game-len (dialogue-length game)))
    (<:p "Select a number between 1 and the one less than the
current turn number.  Moves of the game after the selected number will
be discarded, and the state of the game will be rewound so that the
current turn number is the selected one.")
    (<:form :method "POST"
      (<:table
       (<:tr
	(<:td
	 (<ucw:select :size 1
		      :accessor rewind-point
	   (loop for i from 1 upto (1- game-len)
	      do (<ucw:option :value i (<:as-html i)))))
	(<:td
	 (<ucw:submit :value "Rewind the game to this turn"
		      :action (call 'turn-editor
				    :game (truncate-dialogue game
							     rewind-point))))))))))

(defun render-signature-editor (game)
  (let ((sig (dialogue-signature game)))
    (<:p "The signature being used in this game is:")
    (<:blockquote
     (render sig))
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

(defun render-rule-editor (game)
  (declare (ignore game))
  (<:blockquote
   (<:em "(This functionality is not yet implemented.)"))
  (<ucw:submit :action (+ 1 1)
	       :value "Edit the dialogue rules"))

(defun render-quit-form ()
  (<:p "Quitting the game will discard whatever progress you've made so far and return you to the initial page.")
  (<ucw:form :method "POST"
	     :action (let* ((default-fec (make-instance 'formula-entry-component :signature (copy-signature pqrs-propositional-signature)))
				(default-sgc (make-instance 'start-game-component :formula-entry-component default-fec)))
			   (call 'initial-formula-window :body default-sgc))
	  (<:submit :value "Quit")))
		 

(defmethod render ((self turn-editor))
    (let* ((game (game self))
	   (game-len (dialogue-length game)))
      (<:h1 "The game so far")
      (<:div :style "border:1px solid"
        (render game))
      (<:h1 "Choose from the available moves...")
      (render-available-moves game)
      (<:h1 "...or enter your move manually...")
      (render-manual-move-entry-form game)
      (when (> game-len 1)
	(<:h1 "...or rewind the game...")
	(render-rewind-form game))
      (<:h1 "...or edit the signature...")
      (render-signature-editor game)
      (<:h1 "...or edit the dialogue rules...")
      (render-rule-editor game)
      (<:h1 "...or quit.")
      (render-quit-form)))

(defun render-open-attack (play move-number)
  (with-slots (player statement stance reference)
      play
    (<:tr :style "background-color:#CCCCFF;"
     (<:td (<:as-html move-number))
     (<:td (<:as-html player))
     (<:td (render statement))
     (if (zerop move-number)
	 (<:td (<:em "(initial move)"))
	 (<:td "[" (<:as-html "A") "," (<:as-html reference) "]")))))

(defun render-closed-attack (play move-number)
  (with-slots (player statement stance reference)
      play
    (<:tr :style "background-color:#CCCCCC;"
     (<:td (<:as-html move-number))
     (<:td (<:as-html player))
     (<:td (render statement))
     (if (zerop move-number)
	 (<:td (<:em "(initial move)"))
	 (<:td "[" (<:as-html "A") "," (<:as-html reference) "]")))))

(defun render-defensive-move (play move-number)
  (with-slots (player statement stance reference)
      play
    (<:tr
     (<:td (<:as-html move-number))
     (<:td (<:as-html player))
     (<:td (render statement))
     (if (zerop move-number)
	 (<:td (<:em "(initial move)"))
	 (<:td "[" (<:as-html "D") "," (<:as-html reference) "]")))))

(defmethod render ((game dialogue))
  (unless (zerop (dialogue-length game))
    (let ((open-attacks (open-attack-indices game)))
      (<:table 
       (<:thead
	(<:th "Move")
	(<:th "Player")
	(<:th "Assertion")
	(<:th "Stance, Reference"))
       (<:tbody
	(loop with plays = (dialogue-plays game)
	   with len = (length plays)
	   for play in plays
	   for i from 0 upto len
	   do
	     (if (attacking-move? play)
		 (if (member i open-attacks)
		     (render-open-attack play i)
		     (render-closed-attack play i))
		 (render-defensive-move play i))))
       (<:tfoot
	(<:tr 
	  (<:td :align "center"
		:colspan "4"
		(<:em "Rows in " (<:span :style "background-color:#CCCCCC"
					 "grey")
		      " are closed attacks; rows in "
		      (<:span :style "background-color:#CCCCFF"
			      "blue")
		      " are open attacks.  (Defensive moves are not colored.)"))))))))

(defun render-variable (variable)
  (<:em (<:as-html variable)))

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

(defmethod render ((sa (eql attack-left-conjunct)))
  (<:as-is "&and;")
  (<:sub "L"))

(defmethod render ((sa (eql attack-right-conjunct)))
  (<:as-is "&and;")
  (<:sub "R"))

(defmethod render ((sa (eql which-instance?)))
  (<:as-is "?"))

(defmethod render ((sa (eql which-disjunct?)))
  (<:as-is "?"))

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

(defmethod render ((self game-component))
  (let ((game (game self)))
    (unless (zerop (dialogue-length game))
      (<:table
       (<:thead
	(<:th "Move")
	(<:th "Player")
	(<:th "Assertion")
	(<:th "Stance, Reference"))
       (loop with plays = (dialogue-plays game)
	     with len = (length plays)
	     for play in plays
	     for i from 1 upto len
	  do
	    (with-slots (player statement stance reference)
		play
	      (<:tr 
	       (<:td (<:as-html player))
	       (<:td (<:as-is statement))
	       (if (= i 1)
		   (<:td (<:em "(initial move)"))
		   (<:td "[" (<:as-html stance) "," (<:as-html reference) "]")))))))))

(defcomponent start-game-component ()
  ((formula-entry-component :component t
			    :initarg :formula-entry-component
			    :accessor formula-entry-component)))

(defcomponent formula-entry-component (signature-component)
  ())

(defmethod render ((self formula-entry-component))
  (let (input-formula selected-formula selected-rules)
  (symbol-macrolet 
      (($take-action 
	(let ((sig (signature self)))
	  (if (empty-string? input-formula)
	      (if (belongs-to-signature? sig selected-formula)
		  (call 'turn-editor
			:game (make-dialogue selected-formula 
					     sig
					     selected-rules))
		  (call 'formula-corrector
			:text selected-formula
			:signature sig))
	      (let ((parsed-formula 
		     (ucw-handler-case (parse-formula input-formula)
		       (malformed-formula-error (call 'formula-corrector
						      :text input-formula
						      :signature sig)))))
		(if (belongs-to-signature? sig parsed-formula)
		    (call 'turn-editor
			  :game (make-dialogue 
				 parsed-formula
				 sig
				 selected-rules))
		    (call 'formula-corrector
			  :text parsed-formula
			  :signature sig)))))))
    (let ((sig (signature self)))
      (<:p "To get started, enter a formula in the text box below or choose a famous formula from the menu.")
      (formula-guide)
      (render sig)
      (<:p "You can " (<ucw:a :action (call 'signature-editor :signature sig) "edit the signature") ", if you wish. You will not be able to edit the signature once the game begins.  (If you choose to edit the signature, you'll come back here when you're finished.)")
      (<ucw:form :method "POST"
		 :action $take-action
        (<:p "Enter a formula ")
	(<ucw:input :type "text" :accessor input-formula :id "input-formula")
	(<:p " or select a famous formula from the menu ")
	(<ucw:select :id "selected-formula" 
		     :size 1 
		     :accessor selected-formula
          (dolist (famous-formula famous-formulas)
	    (destructuring-bind (long-name short-name formula)
		famous-formula
	      (declare (ignore short-name))
	      (<ucw:option :value formula (<:as-html long-name)))))
	(<:p
	 (<:as-html "(If you have deleted some elements from the signature but wish to choose one of the pre-selected formulas, you should be aware that the formula you choose might not actually be a formula in a diminished sgnature.)  If the text box is not empty, its contents will be the initial formula.  If the text box is empty, then the selected \"famous formula\" will be used."))
	(<:p "Choose which dialogue rules to use.  The D-dialogue
	rules are the basic dialogue rules.  The E-dialogue rules
	extend the D-dialogue rules and are more strict: they require
	that Opponent always respond to the immediately prior
	assertion of Proponent.")
	(<ucw:select :id "selected-rules"
		     :size 1
		     :accessor selected-rules
	  (<ucw:option :value d-dialogue-rules
		       "D rules (basic rules for intuitionistic logic)")
	  (<ucw:option :value e-dialogue-rules
		       "E rules (D rules + Opponment must always respond immediately)")
	  (<ucw:option :value classical-dialogue-rules
		       "Classical logic rules")
	  (<ucw:option :value d-dialogue-rules-minus-d11
		       "D rules, but you may defend against any open attack")
	  (<ucw:option :value d-dialogue-rules-minus-d12
		       "D rules, but attacks may be answered any number of times"))
	(<:p
	 (<:submit :value "Let's play")))))))

(defmethod render ((self start-game-component))
  (with-slots ((sig signature))
      self
    (render (formula-entry-component self))))

;;; ucw-site.lisp ends here