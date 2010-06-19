;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogues)

(defvar *maintainer-email* "jesse.alama@gmail.com")

(defcomponent signature-editor ()
  ((signature :initarg :signature
	      :accessor signature
	      :type finite-variable-propositional-signature)))

(defaction save-signature ((self signature-editor) new-predicate-symbol)
  (add-predicate (signature self) 
		 (intern-in-dialogue-package new-predicate-symbol)
		 0)
  (answer (signature self)))

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
  ((signature :initarg :signature
	      :accessor signature
	      :type finite-variable-propositional-signature)
   (proposed-name :initarg :name 
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
       (<ucw:a :action (let* ((default-fec (make-instance 'formula-entry-component :signature (copy-signature pqrs-propositional-signature)))
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
		 :action (answer (insert-predicate (signature self)
						   input-predicate-name))
        (<:label :for "new-predicate-name" "New predicate name")
	(<ucw:input :type "text" 
		    :id "new-predicate-name"
		    :accessor input-predicate-name)
	(<:submit :value "Add this predicate")))))

;; deleting a predicate

(defcomponent delete-a-predicate ()
  ((signature :initarg :signature
	      :accessor signature
	      :type finite-variable-propositional-signature)))

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
    (<:p "Predicates:")
     (if (null predicates)
	 (<:em "(none)")
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

(defcomponent formula-corrector ()
  ((text :initarg :text :accessor formula-corrector-text)
   (signature :initarg :signature :accessor formula-corrector-signature)))

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
      (answer (parse-formula formula-str signature))
    (malformed-formula-error ()
      (answer (call 'formula-corrector
		    :text formula-str
		    :signature (formula-corrector-signature self))))))

(defmethod render ((self formula-corrector))
  (let ((input-formula)
	(sig (formula-corrector-signature self))
	(text (formula-corrector-text self)))
    (<:h1 "Invalid formula supplied")
    (<:p "We are unable to make sense of the formula, \"" (render text) "\" that you supplied.  The signature with respect to which you should enter a formula is:")
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

(defcomponent turn-editor ()
  ((game :accessor game
	 :initarg :game)))

(defcomponent game-component ()
  ((game :accessor game
	 :initarg :game)))

(defcomponent turn-evaluator ()
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

(defmethod render ((self turn-evaluator))
  (with-slots (player statement stance reference game)
      self
    (let ((game-len (dialogue-length game)))
    (if (and player statement stance reference)
	(multiple-value-bind (rules-result messages)
	    (evaluate-all-rules (dialogue-rules game)
				game 
				player 
				(dialogue-length game)
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
					  (game self)
					  (make-move player
						     statement
						     stance
						     reference)
					  game-len))
			     "Proceed") "."))
	      (progn
		(<:h1 "Problem!")
		(<:p "The game at this point:")
		(<:p
		 (pretty-print-game game))
		(<:p "Your proposed move:")
		(<:ul
		 (<:li "Player: " (<:as-html player))
		 (<:li "Statement: " (<:as-html statement))
		 (<:li "Stance: " (<:as-html stance))
		 (<:li "Reference: " (<:as-html reference)))
		(<:p "At least one of the dialogue rules is violated by your proposed move:")
		(<:ul
		 (dolist (message messages)
		   (<:li (<:as-html message))))
		(<ucw:form :method "POST"
			   :action (call 'turn-editor :game game)
		  (<ucw:submit :value "Edit this move"
			       :action (call 'turn-editor :game game))))))))))

(defun render-attacks (attacks)
  (dolist (attack attacks)
    (destructuring-bind (statement reference)
	attack
      (<:li (<ucw:a
	     :action (add-move-to-dialogue-at-position game
						       (make-move player
								  statement
								  'a
								  reference)
						       game-len)
	     "Attack move " (<:as-html reference) " by asserting " (render statement))))))

(defun render-defenses (defenses)
  (dolist (defense defenses)
    (destructuring-bind (statement reference)
	defense
      (<:li 
       (<ucw:a :action (add-move-to-dialogue-at-position game
							 (make-move player
								    statement
								    'd
								    reference)
							 game-len)
	       "Defend against the attack of move " (<:as-html reference) " by asserting " (render statement))))))

(defmethod render ((self turn-editor))
  (let (stance-option player-option reference-option input-statement selected-symbolic-attack rewind-point)
    (let* ((game (game self))
	   (game-len (dialogue-length game)))
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
      (<:h1 "The game so far")
      (<:div :style "border:1px solid"
	     (pretty-print-game game))
      (<:h1 "Choose from the available moves...")
      (dolist (player '(p o))
	(<:p "Available moves for " (<:b (if (eq player 'p)
					     "Proponent"
					     "Opponent")) ":")
	(let ((next-attacks (next-attacks game player))
	      (next-defenses (next-defenses game player)))
	  (if (or next-attacks next-defenses)
	      (<:ul
	       (render-attacks next-attacks)
	       (render-defenses next-defenses))
	      (<:p (<:em "(no moves are available.)")))))
      (<:h1 "...or enter your move manually")
      (<:p "The list in the previous section shows all moves that
could be made, by either player, that adhere to the dialogue rules;
follow the links there to make the corresponding moves.  Here, you can
enter a move manually.  If the move you enter is different from any of
the moves in the previous section, then it will inadmissible and you
will see which of the rules are violated by your move.  This is a good
way to explore the meaning of the dialogue rules.")
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
		  (loop for i from 0 upto (1- (dialogue-length game))
		     do (<ucw:option :value i (<:as-html i))))))
	 (<:tr
	  (<:td
	   "What do you want to assert? Enter a formula or choose a symbolic attack.  (If you enter a formula, it will be your proposed assertion; otherwise, the displayed symbolic attack in the menu will be your move.)")
	  (<:td (<ucw:input :type "text"
			    :id "input-statement"
			    :accessor input-statement)
		(<ucw:select :accessor selected-symbolic-attack
			     :size 1
		  (<ucw:option :value 'attack-left-conjunct "Attack the left conjunct")
		  (<ucw:option :value 'attack-right-conjunct "Attack the right conjunct")
		  (<ucw:option :value 'which-disjunct? "Request that a disjunct be chosen")))))
		      
	(<:br)
	(<ucw:submit :value "Make a move"
		     :action $take-action)
	(<:br)
	(let ((length (dialogue-length game)))
	  (when (> length 1)
	    (<:p "or")
	    (<ucw:select :size 1
			 :accessor rewind-point
	      (loop for i from 1 upto (1- (dialogue-length game))
		 do (<ucw:option :value i (<:as-html i))))
	    (<ucw:submit :value "Rewind the game to this turn"
			 :action (call 'turn-editor
				       :game (truncate-dialogue game rewind-point)))))
	(<:p "or")
	(<ucw:submit :value "Quit"
		     :action (let* ((default-fec (make-instance 'formula-entry-component :signature (copy-signature pqrs-propositional-signature)))
				    (default-sgc (make-instance 'start-game-component :formula-entry-component default-fec)))
			       (call 'initial-formula-window :body default-sgc))))))))

(defun pretty-print-game (game)
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
	for i from 0 upto len
	do
	  (with-slots (player statement stance reference)
	      play
	    (<:tr 
	     (<:td (<:as-html i))
	     (<:td (<:as-html player))
	     (<:td (render statement))
	     (if (= i 0)
		 (<:td (<:em "(initial move)"))
		 (<:td "[" (<:as-html stance) "," (<:as-html reference) "]"))))))))

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

(defmethod render ((formula binary-conjunction))
	 (<:as-html "(")
	 (render (lhs formula))
	 (<:as-html " ")
	 (<:as-is "&and;")
	 (<:as-html " ")
	 (render (rhs formula))
	 (<:as-html ")"))

(defmethod render ((formula binary-disjunction))
  (<:as-html "(")
  (render (lhs formula))
  (<:as-html " ")
  (<:as-is "&or;")
  (<:as-html " ")
  (render (rhs formula))
  (<:as-html ")"))

(defmethod render ((formula implication))
  (<:as-html "(")
  (render (antecedent formula))
  (<:as-html " ")
  (<:as-is "&rarr;")
  (<:as-html " ")
  (render (consequent formula))
  (<:as-html ")"))

(defmethod render ((formula equivalence))
   (<:as-html "(")
   (render (lhs formula))
   (<:as-html " ")
   (<:as-is "&harr;")
   (<:as-html " ")
   (render (rhs formula))
   (<:as-html ")"))

(defmethod render ((formula universal-generalization))
  (let ((var (bound-variable formula))
	(body (matrix formula)))
    (<:as-html "(")
    (<:as-is "&forall;")
    (render-variable var)
    (<:as-html "[")
    (<:as-is "&harr;")
    (render body)
    (<:as-html "]")))

(defmethod render ((formula existential-generalization))
  (let ((var (bound-variable formula))
	(body (matrix formula)))
    (<:as-html "(")
    (<:as-is "&exist;")
    (render-variable var)
    (<:as-html "[")
    (<:as-is "&harr;")
    (render body)
    (<:as-html "]")))

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

(defcomponent formula-entry-component ()
  ((signature :initarg :signature
	      :accessor signature
	      :type finite-variable-propositional-signature)))

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
	      (let (parsed-formula)
		(ucw-handler-case (setf parsed-formula
					(parse-formula))
		  (malformed-formula-error (call 'formula-corrector
						 :text input-formula
						 :signature sig)))
		(if (belongs-to-signature? sig parsed-formula)
		    (call 'turn-editor
			  :game (make-dialogue 
				 (parse-formula input-formula) 
				 sig
				 selected-rules))
		    (call 'formula-corrector
			  :text input-formula
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
		       "D rules")
	  (<ucw:option :value e-dialogue-rules
		       "E rules"))
	(<:p
	 (<:submit :value "Let's play")))))))

(defmethod render ((self start-game-component))
  (with-slots ((sig signature))
      self
    (render (formula-entry-component self))))

;;; ucw-site.lisp ends here