;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogues)

;; Server configuration

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dialogue-server-port* 8000))

(defun make-dialogue-backend ()
  (make-backend
   :httpd
   :host "127.0.0.1"
   :port *dialogue-server-port*))

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
    :debug-on-error nil))

(defvar *dialogue-application* (make-instance 'dialogue-application))

(register-application *dialogue-server* *dialogue-application*)

;; adding functions

(defcomponent add-a-function (standard-window-component)
  ((signature :initarg :signature
	      :accessor signature)
   (proposed-name :initarg :name 
		  :accessor proposed-name
		  :initform nil)
   (proposed-arity :initarg :arity 
		   :accessor proposed-arity
		   :initform nil)))

(defmethod render ((self add-a-function))
  (let (input-function-name input-function-arity)
    (symbol-macrolet (($do-over (answer (call 'add-a-function 
					      :signature (signature self)
					      :arity input-function-arity
					      :name input-function-name)))
		    ($take-action (with-slots ((sig signature))
				      self
				    (let ((new-name input-function-name))
				      (if (valid-identifier-name? new-name)
					  (let ((input-function-as-symbol (symbolify new-name)))
					    (if (function? sig input-function-as-symbol)
						$do-over
						(let ((parsed-arity (parse-integer input-function-arity :junk-allowed t)))
						  (if (and parsed-arity (plusp parsed-arity))
						      (answer (add-function sig input-function-as-symbol parsed-arity))
						      $do-over))))
					  $do-over)))))
      (with-slots ((new-name proposed-name) (new-arity proposed-arity) (sig signature))
	  self
	(<:p "The current signature is:")
	(render (signature self))
	(<:p "The new function name should be different from the names of all currently existing functions, constants, and predicates.  It should be different from the empty string and should not contain any whitespace characters.")
	(if (and new-name (not (empty-string? new-name)))
	    (if (contains-whitespace? new-name)
		(progn
		  (<:p "The function name that you gave previously, ")
		  (<:blockquote
		   (<:as-html new-name) ",")
		  (<:p "contains a whitespace character and is unacceptable.  Please try again."))
		(let ((input-function-as-symbol (symbolify new-name)))
		  (if (function? sig input-function-as-symbol)
		      (progn
			(<:p "The function name that you gave previously, ")
			(<:blockquote
			 (<:as-html new-name) ",")
			(<:p "is already defined in the given signature and cannot be overwritten.  Please try again."))))))
	(if new-arity
	    (let ((parsed-arity (parse-integer new-arity :junk-allowed t)))
	      (if (null parsed-arity)
		  (progn
		    (<:p "The arity")
		    (<:blockquote
		     (<:as-html new-arity))
		    (<:p "that you specified for the new function " (<:as-html new-name) " could not be understood as a number.  Please try again."))
		  (unless (plusp parsed-arity)
		    (<:p "The arity")
		    (<:blockquote
		     (<:as-html new-arity))
		    (<:p "that you specified for the new function " (<:as-html new-name) " is not a positive integer.  Please try again.")))))
	(<ucw:form :method "POST"
		   :action $take-action
	  (<:label :for "new-function-name" "New function name")
	  (<ucw:input :type "text"
		      :id "new-function-name"
		      :accessor input-function-name)
	  (<:br)
	  (<:label :for "new-function-arity" "New function arity")
	  (<ucw:input :type "text"
		      :id "new-function-arity"
		      :accessor input-function-arity)
	  (<:br)
	  (<ucw:submit :value "Add this function"
		       :action $take-action))))))

;; deleting functions

(defcomponent delete-a-function (standard-window-component)
  ((signature :initarg :signature
	      :accessor signature)))

(defmethod render ((self delete-a-function))
  (let (selected-function)
    (symbol-macrolet (($take-action (answer (delete-function (signature self) selected-function))))
      (with-slots ((sig signature))
	  self
	(<:h1 "Deleting a function")
	(if (signature-functions sig)
	    (<ucw:form :method "POST"
		       :action $take-action
	      (<:p "Choose a function to be deleted from the signature:")
	      (<ucw:select :size 1
			   :accessor selected-function
	        (dolist (function-and-arity (signature-functions sig))
		  (let ((function (first function-and-arity)))
		    (<ucw:option :value function
				 (<:as-html function)))))
	      (<ucw:submit :value "Delete this function"
			   :action $take-action))
	    (<:p "There are no functions in the signature; none can be deleted. " (<ucw:a :action (answer (signature self))
											  "Proceed") "."))))))

;; adding a predicate

(defcomponent add-a-predicate (standard-window-component)
  ((signature :initarg :signature
	      :accessor signature)
   (proposed-name :initarg :name :accessor proposed-name :initform nil)
   (proposed-arity :initarg :arity :accessor proposed-arity :initform nil)))

(defmethod render ((self add-a-predicate))
  (let (input-predicate-name input-predicate-arity)
    (symbol-macrolet (($do-over (answer (call 'add-a-predicate
					      :signature (signature self)
					      :arity input-predicate-arity
					      :name input-predicate-name)))
		      ($take-action (let ((new-name input-predicate-name))
				      (if (valid-identifier-name? new-name)
					  (let ((input-predicate-as-symbol (symbolify new-name)))
					    (if (predicate? (signature self) input-predicate-as-symbol)
						$do-over
						(let ((parsed-arity (parse-integer input-predicate-arity :junk-allowed t)))
						  (if (and parsed-arity (not (minusp parsed-arity)))
						      (answer (add-predicate (signature self) input-predicate-as-symbol parsed-arity))
						      $do-over))))
					  $do-over))))
      (with-slots ((new-name proposed-name) (new-arity proposed-arity) (sig signature))
	  self
	(<:p "The current signature is:")
	(render sig)
	(<:p "The new predicate name should be different from the names of all currently existing predicates, constants, and predicates.  It should be different from the empty string and should not contain any whitespace characters.")
	(if (and new-name (not (empty-string? new-name)))
	    (if (contains-whitespace? new-name)
		(progn
		  (<:p "The predicate name that you gave previously, ")
		  (<:blockquote
		   (<:as-html new-name) ",")
		  (<:p "contains a whitespace character and is unacceptable.  Please try again."))
		(let ((input-predicate-as-symbol (symbolify new-name)))
		  (if (predicate? sig input-predicate-as-symbol)
		      (progn
			(<:p "The predicate name that you gave previously, ")
			(<:blockquote
			 (<:as-html new-name) ",")
			(<:p "is already defined in the given signature and cannot be overwritten.  Please try again."))))))
	(if new-arity
	    (let ((parsed-arity (parse-integer new-arity :junk-allowed t)))
	      (if (null parsed-arity)
		  (progn
		    (<:p "The arity")
		    (<:blockquote
		     (<:as-html new-arity))
		    (<:p "that you specified for the new predicate " (<:as-html new-name) " could not be understood as a number.  Please try again."))
		  (unless (minusp parsed-arity)
		    (<:p "The arity")
		    (<:blockquote
		     (<:as-html new-arity))
		    (<:p "that you specified for the new predicate " (<:as-html new-name) " is not a natural number.  Please try again.")))))
	(<ucw:form :method "POST"
		   :action $take-action
          (<:label :for "new-predicate-name" "New predicate name")
	  (<ucw:input :type "text"
		      :id "new-predicate-name"
		      :accessor input-predicate-name)
	  (<:br)
	  (<:label :for "new-predicate-arity" "New predicate arity")
	  (<ucw:input :type "text"
		      :id "new-predicate-arity"
		      :accessor input-predicate-arity)
	  (<:br)
	  (<ucw:submit :value "Add this predicate"
		       :action $take-action))))))

;; deleting a predicate

(defcomponent delete-a-predicate (standard-window-component)
  ((signature :initarg :signature
	      :accessor signature)))

(defmethod render ((self delete-a-predicate))
  (let (selected-predicate)
    (symbol-macrolet (($take-action (answer (delete-predicate (signature self) selected-predicate))))
      (with-slots ((sig signature))
	  self
	(<:h1 "Deleting a predicate")
	(if (signature-predicates sig)
	    (<ucw:form :method "POST"
		       :action $take-action
	      (<:p "Choose a predicate to be deleted from the signature:")
	      (<ucw:select :size 1
			   :accessor selected-predicate
	        (dolist (predicate-and-arity (signature-predicates sig))
		  (let ((pred (first predicate-and-arity)))
		    (<ucw:option :value pred
				 (<:as-html pred)))))
	      (<ucw:submit :value "Delete this predicate"
			   :action $take-action))
	    (<:p "There are no predicates in the signature; none can be deleted. " (<ucw:a :action (answer (signature self))
											  "Proceed") "."))))))

;; adding a constant

(defcomponent add-a-constant (standard-window-component)
  ((signature :initarg :signature
	      :accessor signature)
   (proposed-name :initarg :name :accessor proposed-name :initform nil)))

(defmethod render ((self add-a-constant))
  (let (input-constant-name)
    (symbol-macrolet (($do-over (answer (call 'add-a-constant
					      :signature (signature self)
					      :name input-constant-name)))
		      ($take-action (if (valid-identifier-name? input-constant-name)
					(let ((input-constant-as-symbol (symbolify input-constant-name)))
					  (if (constant? (signature self) input-constant-as-symbol)
					      $do-over
					      (answer (add-constant (signature self) input-constant-as-symbol))))
					$do-over)))
      (with-slots ((new-name proposed-name) (sig signature))
	  self
	(<:p "The current signature is:")
	(render sig)
	(<:p "The new constant name should be different from the names of all currently existing constants, constants, and predicates.  It should be different from the empty string and should not contain any whitespace characters.")
	(if (and new-name (not (empty-string? new-name)))
	    (if (contains-whitespace? new-name)
		(progn
		  (<:p "The constant name that you gave previously, ")
		  (<:blockquote
		   (<:as-html new-name) ",")
		  (<:p "contains a whitespace character and is unacceptable.  Please try again."))
		(let ((input-constant-as-symbol (symbolify new-name)))
		  (if (constant? sig input-constant-as-symbol)
		      (progn
			(<:p "The constant name that you gave previously, ")
			(<:blockquote
			 (<:as-html new-name) ",")
			(<:p "is already defined in the given signature and cannot be overwritten.  Please try again."))))))
	(<ucw:form :method "POST"
		   :action $take-action
	  (<:label :for "new-constant-name" "New constant name")
	  (<ucw:input :type "text"
		      :id "new-constant-name"
		      :accessor input-constant-name)
	  (<:br)
	  (<ucw:submit :value "Add this constant"
		       :action $take-action))))))

;; deleting a constant

(defcomponent delete-a-constant (standard-window-component)
  ((signature :initarg :signature
	      :accessor signature)))

(defmethod render ((self delete-a-constant))
  (let (selected-constant)
    (symbol-macrolet (($take-action (answer (delete-constant (signature self) selected-constant))))
      (with-slots ((sig signature))
	  self
	(<:h1 "Deleting a constant")
	(if (signature-constants sig)
	    (<ucw:form :method "POST"
		       :action $take-action
	      (<:p "Choose a constant to be deleted from the signature:")
	      (<ucw:select :size 1
			   :accessor selected-constant
	        (dolist (constant (signature-constants sig))
		  (<ucw:option :value constant
			       (<:as-html constant))))
	      (<ucw:submit :value "Delete this predicate"
			   :action $take-action))
	    (<:p "There are no constants in the signature; none can be deleted. " (<ucw:a :action (answer (signature self))
											  "Proceed") "."))))))

(defcomponent signature-editor ()
  ((signature :initarg :signature
	      :accessor signature)))

(defmethod render ((self signature-editor))
  (with-slots ((sig signature))
      self
    (<:p "The signature that will be used during the game is:")
    (render sig)
    (<:p "You can:")
    (<:ul
     (<:li (<ucw:a :action (call 'add-a-function :signature sig) "add a function") ",")
     (<:li (<ucw:a :action (call 'delete-a-function :signature sig) "delete a function") ",")
     (<:li (<ucw:a :action (call 'add-a-constant :signature sig)  "add a constant") ",")
     (<:li (<ucw:a :action (call 'delete-a-constant :signature sig)  "delete a constant") ",")
     (<:li (<ucw:a :action (call 'add-a-predicate :signature sig) "add a predicate") ", or")
     (<:li (<ucw:a :action (call 'delete-a-predicate :signature sig) "delete a predicate") "."))
    (<:p
     "When you're satisfisfied with the signature, you may "
     (<ucw:a :action (answer sig) "proceed") " (you will return to wherever you were before you arrived here at the signature editor).")))

(defmethod render ((self signature))
  (with-slots (constants functions predicates)
      self
    (<:dl
     (<:dt "Constants")
     (if (null constants)
	 (<:dd (<:em "(none)"))
	 (<:dd (<:as-html (comma-separated-list constants))))
     (<:dt "Functions")
     (if (null functions)
	 (<:dd (<:em "(none)"))
	 (<:dd (<:table
		(<:thead
		 (<:th "Name")
		 (<:th "Arity"))
		(dolist (name-and-arity functions)
		  (destructuring-bind (name . arity)
		      name-and-arity
		    (<:tr
		     (<:td (<:as-html name))
		     (<:td (<:as-html arity))))))))
     (<:dt "Predicates")
     (if (null predicates)
	 (<:dd (<:em "(none)"))
	 (<:dd (<:table
		(<:thead
		 (<:th "Name")
		 (<:th "Arity"))
		(dolist (name-and-arity predicates)
		  (destructuring-bind (name . arity)
		      name-and-arity
		    (<:tr
		     (<:td (<:as-html name))
		     (<:td (<:as-html arity)))))))))))

(defentry-point "" (:application *dialogue-application*)
    ()
  (call 'initial-formula-window :signature pqrs-propositional-signature))

(defcomponent initial-formula-window (standard-window-component)
  ((signature :accessor signature
	      :initarg :signature))
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

(defcomponent number-corrector (standard-window-component)
  ((num :initarg :number
	:accessor number-corrector-num)))

(defmethod render ((self number-corrector))
  (let (input-number) ;; annoying that I have to lexically bind this 
    (symbol-macrolet (($take-action (let ((parsed-number (parse-integer input-number :junk-allowed t)))
				      (if (null parsed-number)
					  (answer (call 'number-corrector :number input-number))
					  (answer parsed-number)))))
      
      (<:h1 "Invalid number supplied")
      (<:p "We are unable to make sense of the number, \"" (<:as-html (number-corrector-num self)) "\" that you supplied.  Please try again.")
      (<ucw:form :method "POST"
		 :action $take-action
      (<:label :for "number-input" "Enter a non-negative integer")
      (<ucw:input :type "text"
		  :id "number-input"
		  :accessor input-number)
      (<ucw:submit :value "Use this number"
		   :action $take-action)))))

(defcomponent formula-corrector (standard-window-component)
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

(defmethod render ((self formula-corrector))
  (symbol-macrolet (($take-action (handler-case (answer (parse-formula input-formula (formula-corrector-signature self)))
				    (malformed-formula-error () (call 'formula-corrector 
								      :text input-formula
								      :signature (formula-corrector-signature self))))))
    (let (input-formula)
      (<:h1 "Invalid formula supplied")
      (<:p "We are unable to make sense of the formula, \"" (<:as-html (formula-corrector-text self)) "\" that you supplied.  The signature with respect to which you should enter a formula is:")
      (render (formula-corrector-signature self))
      (formula-guide)
      (<:p "Please try again.")
      (<ucw:form :method "POST"
		 :action $take-action
	(<:p "Enter a formula in the above signature.  If you wish, you can " (<ucw:a :action (let ((new-signature (call 'signature-editor :signature (formula-corrector-signature self))))
												(handler-case (answer (parse-formula (formula-corrector-text self) new-signature))
												    (malformed-formula-error () (answer (call 'formula-corrector :text input-formula)))))
										      "edit the signature") ". (If edit the signature and the formula that you provided becomes well-formed in the new signature, then you will go back to where you were before you came here.  If, after editing the signature, the formula is still not valid, you will come back to this page.)")
	(<ucw:input :type "text"
		    :id "formula-input"
		    :accessor input-formula)
	(<ucw:submit :value "Use this formula"
		     :action $take-action)))))

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
	 :initarg :game)))

(defmethod render ((self game-viewer))
  (with-slots (player statement stance reference game)
      self
    (when (and player statement stance reference)
      (multiple-value-bind (rules-result messages)
	  (evaluate-all-rules d-dialogue-rules 
			      game 
			      player 
			      (dialogue-length game)
			      statement 
			      stance
			      reference)
	(if rules-result
	    (progn
	      (add-move-to-dialogue (game self)
				    (make-move player
					       statement
					       stance
					       reference))
	      (setf (player self) nil
		    (statement self) nil
		    (stance self) nil
		    (reference self) nil))
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
			 :action (call 'game-viewer :game game)
	        (<ucw:submit :value "Edit this move"
			     :action (call 'game-viewer :game game)))))))
    (let (stance-option player-option
	  input-reference input-statement)
      (symbol-macrolet (($take-action (progn
					(if input-statement
					    (handler-case (setf statement (parse-formula input-statement))
					      (malformed-formula-error () (setf statement
										(call 'formula-corrector
										      :text input-statement
										      :signature (dialogue-signature game)))))
					    
					    (if input-reference
						(let ((parsed-integer (parse-integer input-reference :junk-allowed t)))
						  (if (null parsed-integer)
						      (setf reference (call 'number-corrector :number input-reference))
						      (setf reference parsed-integer)))))
					(call 'game-viewer
					      :player (or player player-option)
					      :statement (or statement
							     input-statement)
					      :stance (or stance stance-option)
					      :reference (or reference
							     input-reference)
					      :game game))))
	(<:h1 "The game so far")
	(<:div :style "border:1px solid"
	       (pretty-print-game game))
	(<ucw:form :method "POST"
		   :action $take-action
		   (cond ((not player)
			  (<:p "Which player will move?")
			  (<ucw:select :accessor player-option
				       :size 1
			    (<ucw:option :value 'p "Proponent")
			    (<ucw:option :value 'o "Opponent"))
			  (<ucw:submit :value "Choose sides"
				       :action $take-action))
			 ((not stance)
			  (<:p "Choose whether to attack or defend.")
			  (<ucw:select :accessor stance-option
				       :size 1
			    (<ucw:option :value 'a "Attack")
			    (<ucw:option :value 'd "Defend"))
			  (<ucw:submit :value "Make a move"
				       :action $take-action))

			 ((not reference)
			  (<:p "To which statement do you want to respond? (Your answer should be a non-negative integer.)")
			  (<ucw:input :type "text"
				      :id "input-reference"
				      :accessor input-reference)
			  (<:br)
			  (<ucw:submit :value "Respond"
				       :action $take-action))
			 (t ;; we have to get a statement
			  (<:p "What do you want to assert? ")
			  (<ucw:input :type "text"
				      :id "input-statement"
				      :accessor input-statement)
			  (<:br)
			  (<ucw:submit :value "Make a claim"
				       :action $take-action))))))))

;; I'm confused about what to do here.  I want the user to indicate,
;; first of all, whether they should attack or defend something.  I
;; think I can do that.  But what component do I call, once I have the
;; information about what kind of stance the user wants to take?  The
;; same component that displays the game?  Do I need to define a
;; new entry point?

(defun pretty-print-game (game)
  (unless (zerop (dialogue-length game))
    (<:table
     (<:thead
      (<:th "Move Number")
      (<:th "Player")
      (<:th "Assertion")
      (<:th "Stance and Reference"))
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
	     (<:td (<:as-is statement))
	     (if (= i 0)
		 (<:td (<:em "(initial move)"))
		 (<:td "[" (<:as-html stance) "," (<:as-html reference) "]"))))))))

(defmethod render ((self game-component))
  (let ((game (game self)))
    (unless (zerop (dialogue-length game))
      (<:table
       (<:thead
	(<:th "Move Number")
	(<:th "Player")
	(<:th "Assertion")
	(<:th "Stance and Reference"))
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

(defmethod render ((self initial-formula-window))
  (let (input-formula selected-formula)
    (symbol-macrolet (($take-action (let ((sig (signature self)))
				      (if (empty-string? input-formula)
					  (if (formula? selected-formula sig)
					      (call 'game-viewer
						    :game (make-dialogue selected-formula sig))
					      (call 'formula-corrector
						    :text (format nil "~A" selected-formula)
						    :signature sig))
					  (handler-case (call 'game-viewer
							      :game (make-dialogue (parse-formula input-formula sig)
										   sig))
					    (malformed-formula-error (call 'formula-corrector
								      :text input-formula
								      :signature sig)))))))
      (with-slots ((sig signature))
	  self
	(<:h1 "It's your turn")
	(<:p "To get started, enter a formula in the text box below or choose a famous formula from the menu.")
	(formula-guide)
	(render sig)
	(<:p "You can " (<ucw:a :action (call 'signature-editor :signature sig) "edit the signature") ", if you wish. (If you choose to edit the signature, you'll come back here when you're finished.)")
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
	  (<:p
	   (<ucw:submit :action $take-action
			:value "Let's play")))))))

;;; ucw-site.lisp ends here