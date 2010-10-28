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
	d-dialogue-rules-literal-d10
	e-dialogue-rules-literal-d10
	d-dialogue-rules-inverted
	e-dialogue-rules-inverted
	classical-dialogue-rules-inverted
	only-particle-rules
	particle-rules+d10
	particle-rules+d11
	particle-rules+d12
	particle-rules+d13
	particle-rules+e))

(defparameter available-translations
  (list identity-translation
	goedel-gentzen-translation 
	double-negate-translation
	double-negate-all-subformulas-translation
	kuroda-translation
	negate-atomic-subformulas-translation
	double-negate-atomic-subformulas-translation
	self-conjoin-atomic-subformulas-translation
	self-disjoin-atomic-subformulas-translation
	contrapositivify-translation
	atomic->excluded-middle-translation
	converse-translation))

(defclass ruleset-component ()
  ((ruleset :initarg :ruleset
	    :initform nil
	    :accessor ruleset
	    :type (or (eql nil) ruleset))))

(defclass signature-component ()
  ((signature :initarg :signature
	      :initform alphabetic-propositional-signature
	      :type finite-variable-propositional-signature
	      :accessor signature)))

(defclass game-component ()
  ((game :accessor game
	 :initarg :game
	 :initform nil
	 :type (or (eql nil) dialogue))))

(defclass play-style-component ()
  ((play-style :accessor play-style
	       :initarg :play-style
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
       (<ucw:a :action (call 'start-game-component)
	       "quit and start over") "."))

;; (defgeneric render-signature (signature))

;; (defmethod render-signature ((sig finite-variable-propositional-signature))
;;   (<:em "Predicates: ")
;;   (with-slots (predicates) sig
;;     (if (null predicates)
;; 	(<:em "(none)")
;; 	(let ((first (car predicates)))
;; 	  (<:em (<:as-html first))
;; 	  (dolist (pred (cdr predicates))
;; 	    (<:as-is ", ")
;; 	    (<:em (<:as-html pred)))))))

(defentry-point "" (:application *dialogue-application*)
    ()
    (call 'initial-formula-window))

(defcomponent about-component ()
  ())

(defmethod render ((self about-component))
  (<:h1 "About dialogue games and dialogical logic")
  (<:p "Lorenzen dialogues are, originaly, a formalism for capturing
intuitionistic validity using games.  Since their invention and
development in the late 1950s and 1960s, they have been extended from
intuitionistic first-order logic so that they apply to different
notions of validity, such as those of classical logic, modal logics,
linear logic, etc.  For more information,
consult " (<:a :href "http://plato.stanford.edu/entries/logic-dialogical/" "the
entry on dialogue games") " in the " (<:em "Stanford Encyclopedia of
Philosophy") ".")
  (<:p "There's also " (<:a :href "http://dialogue-games.blogspot.com/" :title "TONIGHT: Proponent v. Opponent" "a blog") " about dialogue games and dialogical logic, to which the maintainer of this web site contributes.")
  (<:p "The " (<:a :href "http://www-ls.informatik.uni-tuebingen.de/difos/" :title "Dialogical Foundations of Semantics" "homepage for the research project for which this site was designed") " also contains some valuable information.")
  (<:hr)
  (<:address
   (<:a :href "mailto:jesse.alama@gmail.com"
	"Contact the site maintainer")))

(defcomponent initial-formula-window (standard-window-component)
  ()
  (:default-initargs
      :title "play a lorenzen dialogue game"
      :doctype yaclml:+xhtml-strict-doctype+
      :body
      (make-instance 'tabbed-pane
		     :current-component-key "play a game"
		     :key-test #'string=
		     :contents `(("play a game" . ,(make-instance 'start-game-component))
				 ("about dialogical logic" . ,(make-instance 'about-component))
				 ("about this site" . ,(make-instance 'about-this-site-component))
				 ("contact" . ,(make-instance 'contact-page))))))

(defcomponent contact-page ()
  ())

(defmethod render ((self contact-page))
  (<:h1 "Contact")
  (<:p "Questions? Bugs? Feature requests?  Comments?  You're welcome to email the site maintainer; use the address below." )
  (<:blockquote
   (<:address
    (<:a :href "mailto:jesse.alama@gmail.com"
	 "jesse.alama@gmail.com"))))

(defcomponent about-this-site-component ()
  ())

(defmethod render ((self about-this-site-component))
  (<:h1 "About this site")
  (<:h2 "History and motivation")
  (<:p "This site was created as an attempt to understand dialogue
games and dialogical logic.")
  (<:p "Playing these games by hand, with pencil and paper, often led
to pitfalls because I would often fail to see some possible moves that
could be made in a game, owing to the awkward nature of some of the
rules that are commonly used.  I thus wrote some software for playing
dialogue games that helped me to play them and make statements with
confidence about their properties.")
  (<:p "A web application on top of what I had made seemed valuable to me for various reasons:")
  (<:ul
   (<:li (<:b "disseminating dialogical games") " in the world of
mathematical and philosophical logic.  If some student somewhere plays
just one or two dialogue games and gets hooked, this site will have
served this purpose;")
   (<:li (<:b "enabling collaboration") " between researchers working
on dialogical logic.  The idea is to be able to play concrete games or
search for winning strategies and share the results with other
researchers.  Different researchers would have a common, reliable,
objective presentation of the games, so as to learn from or resolve
disputes about them;"))
  (<:p "(Of course, as a hacker, I also wanted to
just " (<:em "make") " this thing.  But perhaps that goes without
saying.)")
  (<:h2 "Implementation")
  (<:p "This site was written in Common Lisp using
the " (<:a :href "http://common-lisp.net/project/ucw/" :title "UnCommon
Web" "UnCommon Web") " system.  The Common Lisp implementation is " (<:a :href "http://www.sbcl.org" :title "Steel Bank Common Lisp" "SBCL") ".  You can follow the development on " (<:a :href "http://github.com/jessealama/dialogues" :title "dialogues on github" "github") "."))

(defparameter famous-formulas
  `(("Peirce's formula" "peirce-formula" ,peirce-formula)
    ("Excluded middle" "excluded-middle" ,excluded-middle)
    ("Weak excuded middle" "weak-excluded-middle" ,weak-excluded-middle)
    ("Conditional excluded middle" "conditional-excluded-middle" ,conditional-excluded-middle)
    ("Dummett's formula" "dummett-formula" ,dummett-formula)
    ("Double negation introduction" "double-negation-introduction" ,double-negation-introduction)
    ("Double negation elimination" "double-negation-elimination" ,double-negation-elimination)
    ("K formula" "k-formula" ,k-formula)
    ("B formula" "b-formula" ,b-formula)
    ("C formula" "c-formula" ,c-formula)
    ("W formula" "w-formula" ,w-formula)
    ("Scott's formula" "scott-formula" ,scott-formula)
    ("Smetanich's formula" "smetanich-formula" ,smetanich-formula)
    ("De Morgan &not;(P &and; Q) &rarr; (&not;P &or; &not;Q)" "de-morgan-not-and-implies-or-not" ,de-morgan-not-and-implies-or-not)
    ("De Morgan &not;(P &or; Q) &rarr; (&not;P &and; &not;Q)" "de-morgan-not-or-implies-and-not" ,de-morgan-not-or-implies-and-not)
    ("De Morgan (&not;P &and; &not;Q) &rarr; &not;(P &or; Q)" "de-morgan-and-not-implies-not-or" ,de-morgan-and-not-implies-not-or)
    ("De Morgan (&not;P &or; &not;Q) &rarr; &not;(P &and; Q)" "de-morgan-or-not-implies-not-and" ,de-morgan-or-not-implies-not-and)
    ("(P &rarr; &not;P) &or; (&not;P &rarr; P)" "anti-connexive-formula" ,anti-connexive-formula)
    ("Ex contradictione quodlibet" "ex-contradictione-quodlibet" ,ex-contradictione-quodlibet)
    ("Implicational ex falso quodlibet" "implicational-ex-falso" ,implicational-ex-falso)
    ("WKP" "wkp" ,wkp)
    ("Aristotle's thesis (positive antecedent)" "aristotles-thesis-positive-antecedent" ,aristotles-thesis-positive-antecedent)
    ("Aristotle's thesis (negative antecedent)" "aristotles-thesis-negative-antecedent" ,aristotles-thesis-negative-antecedent)
    ("Modus ponens" "modus-ponens" ,modus-ponens)
    ("Modus tollens" "modus-tollens" ,modus-tollens)
    ("Hypothetical syllogism" "hypothetical-syllogism" ,hypothetical-syllogism)
    ("Disjunctive syllogism" "disjunctive-syllogism" ,disjunctive-syllogism)
    ("Constructive dilemma" "constructive-dilemma" ,constructive-dilemma)
    ("Destructive dilemma" "destructive-dilemma" ,destructive-dilemma)
    ("Conjunction elimination" "conjunction-elimination" ,conjunction-elimination)
    ("Conjunction introduction" "conjunction-introduction" ,conjunction-introduction)
    ("Disjunction introduction" "disjunction-introduction" ,disjunction-introduction)
    ("Composition" "composition" ,composition)
    ("Commutativity of conjunction" "commutivity-of-conjunction" ,commutivity-of-conjunction)
    ("Commutativity of disjunction" "commutativity-of-disjunction" ,commutativity-of-disjunction)
    ("Commutativity of implication" "commutativity-of-implication" ,commutativity-of-implication)
    ("Associativity of conjunction" "associativity-of-conjunction" ,associativity-of-conjunction)
    ("Associativity of disjunction" "associativity-of-disjunction" ,associativity-of-disjunction)
    ("Associativity of implication" "associativity-of-implication" ,associativity-of-implication)
    ("Distributivity of conjunction over disjunction (conjunctive antecdent)" "distributivity-of-conjunction-over-disjunction-conjunctive-antecedent" ,distributivity-of-conjunction-over-disjunction-conjunctive-antecedent)
    ("Distributivity of conjunction over disjunction (disjunctive antecdent)" "distributivity-of-conjunction-over-disjunction-disjunctive-antecedent" ,distributivity-of-conjunction-over-disjunction-disjunctive-antecedent)
    ("Distributivity of disjunction over conjunction (disjunctive antecedent)" "distributivity-of-disjunction-over-conjunction-disjunctive-antecedent" ,distributivity-of-disjunction-over-conjunction-disjunctive-antecedent)
    ("Distributivity of disjunction over conjunction (conjunctive antecedent)" "distributivity-of-disjunction-over-conjunction-conjunctive-antecedent" ,distributivity-of-disjunction-over-conjunction-conjunctive-antecedent)
    ("Transposition" "transposition" ,transposition)
    ("Material implication (implicational antecdent)" "material-implication-implicational-antecedent" ,material-implication-implicational-antecedent)
    ("Material implication (disjunctive antecdent)" "material-implication-disjunctive-antecedent" ,material-implication-disjunctive-antecedent)
    ("False material implication (negative antecedent)" "material-implication-negative-antecedent" ,material-implication-negative-antecedent)
    ("False material implication (negative consequent)" "material-implication-negative-consequent" ,material-implication-negative-consequent)
    ("Material equivalence (conjunctive antecedent)" "material-equivalence-conjunctive-antecedent" ,material-equivalence-conjunctive-antecedent)
    ("Material equivalence (disjunctive antecedent)" "material-equivalence-disjunctive-antecedent" ,material-equivalence-disjunctive-antecedent)
    ("Exportation (conjunctive antecedent)" "exportation-conjunctive-antecedent" ,exportation-conjunctive-antecedent)
    ("Exportation (implicational antecedent)" "exportation-implicational-antecedent" ,exportation-implicational-antecedent)

    ("Idempotency of conjunction (conjunctive antecedent)" "conjunctive-idempotency-conjunctive-antecedent" ,conjunctive-idempotency-conjunctive-antecedent)
    ("Idempotency of conjunction (conjunctive consequent)" "conjunctive-idempotency-conjunctive-consequent" ,conjunctive-idempotency-conjunctive-consequent)
    ("Idempotency of disjunction (disjunctive antecedent)" "disjunctive-idempotenency-disjunctive-antecedent" ,disjunctive-idempotenency-disjunctive-antecedent)
    ("Idempotenc of disjunction (disjunctive consequent)" "disjunctive-idempotenency-disjunctive-consequent" ,disjunctive-idempotenency-disjunctive-consequent)
    ("Disjunctive absorption (disjunctive antecedent)" "disjunctive-absorption-disjunctive-antecedent" ,disjunctive-absorption-disjunctive-antecedent)
    ("Disjunctive absorption (disjunctive consequent)" "disjunctive-absorption-disjunctive-consequent" ,disjunctive-absorption-disjunctive-consequent)
    ("Conjunctive absorption (conjunctive antecdent)" "conjunctive-absorption-conjunctive-antecedent" ,conjunctive-absorption-conjunctive-antecedent)
    ("Conjunctive absorption (conjunctive consequent)" "conjunctive-absorption-conjunctive-consequent" ,conjunctive-absorption-conjunctive-consequent)
    ("Frege formula" "frege-formula" ,frege-formula)
    ("Contrapositive (positive antecedent)" "contraposition-positive-antecedent" ,contraposition-positive-antecedent)
    ("Contrapositive (negative antecedent)" "contraposition-negative-antecedent" ,contraposition-negative-antecedent)
    ("McColl's Connexive Axiom 1" "connexive-ax-1" ,connexive-ax-1)
    ("McColl's Connexive Axiom 2" "connexive-ax-2" ,connexive-ax-2)
    ("McColl's Connexive Axiom 3" "connexive-ax-3" ,connexive-ax-3)
    ("McColl's Connexive Axiom 4" "connexive-ax-4" ,connexive-ax-4)
    ("McColl's Connexive Axiom 5" "connexive-ax-5" ,connexive-ax-5)
    ("McColl's Connexive Axiom 6" "connexive-ax-6" ,connexive-ax-6)
    ("McColl's Connexive Axiom 7" "connexive-ax-7" ,connexive-ax-7)
    ("McColl's Connexive Axiom 8" "connexive-ax-8" ,connexive-ax-8)
    ("McColl's Connexive Axiom 9" "connexive-ax-9" ,connexive-ax-9)
    ("McColl's Connexive Axiom 10" "connexive-ax-10" ,connexive-ax-10)
    ("McColl's Connexive Axiom 11" "connexive-ax-11" ,connexive-ax-11)
    ("McColl's Connexive Axiom 12" "connexive-ax-12" ,connexive-ax-12)))

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
  (<:p "Atomic formulas are simply the letters of the alphabet A, B, " (<:as-is "&hellip;") ", Z.  The case you use to write connectives and atomic formulas doesn't matter (anything you enter will be upcased).")
  (<:p "Here are some " (html-quote "famous formulas") " that can be referred to by name:")
  (<:table :rules "all"
   (<:thead
    (<:tr
     (<:th "Name")
     (<:th "Identifier")
     (<:th "Value")))
   (<:tbody :style "border:1px solid;"
   (dolist (famous-formula famous-formulas)
     (destructuring-bind (long-name identifier-name value)
	 famous-formula
       (<:tr
	(<:td (<:as-is long-name))
	(<:td (<:tt (<:as-is identifier-name)))
	(<:td (render value)))))))
  (<:p "When constructing formulas manually, you can refer to these famous formulas by simply using their identifier name.  Example:")
  (<:blockquote
   (<:tt "(implies excluded-middle ex-contradictione-quodlibet)"))
  (<:p "will be interpreted as")
  (<:blockquote
   (render (-> excluded-middle ex-contradictione-quodlibet)))
  (<:p "The famous formulas are rigidly defined: " (<:tt "peirce-formula") ", for example, refers to a specific formula composed of specific atomic subformulas in a fixed order.  If you want to express things such as " (<:tt "(or q (not q))") ", an instance of the excluded middle with the variable " (<:em "q") " instead of the variable " (<:em "p") ", then you have to type it manually; at present there is no way to influence the name of the atomic subformulas nor their order."))

(defaction parse-formula-action (formula-str signature)
  (answer
   (ucw-handler-case
       (parse-formula formula-str)
     (end-of-file ()
		  (call 'formula-corrector
			:text formula-str
			:signature signature))
     (malformed-formula-error ()
       (call 'formula-corrector
	     :text formula-str
	     :signature signature)))))

(defmethod render ((self formula-corrector))
  (let ((input-formula)
	(sig (signature self))
	(text (formula-corrector-text self)))
    (<:h1 "Invalid formula supplied")
    (<:p "We are unable to make sense of the formula, \""
	 (if (stringp text)
	     (<:as-html text)
	     (if (null text)
		 (<:as-is "(weird -- NIL supplied)")
		 (render text))) "\" that you supplied.")
    ;; (render-signature sig)
    (formula-guide)
    (<:p "Please try again.")
    (<ucw:form :method "post"
	       :action (parse-formula-action input-formula sig)
      (<:p "Enter a formula in the above signature.")
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
will inadmissible and you will see which of the rules are violated by
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
			  (<ucw:option :value attack-left-conjunct 
				       "Attack the left conjunct")
			  (<ucw:option :value attack-right-conjunct
				       "Attack the right conjunct")
			  (<ucw:option :value which-disjunct?
				       "Request that a disjunct be chosen")
		          (<ucw:option :value attack-left-disjunct 
				       "Attack the left disjunct")
			  (<ucw:option :value attack-right-disjunct
				       "Attack the right disjunct")
			  (<ucw:option :value which-conjunct?
				       "Request that a conjunct be chosen"))))
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
        (render-game game :moves-to-highlight (mapcar #'car indices-and-violated-rules)))
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

(defcomponent winning-strategy-searcher (game-component play-style-component)
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
	         (render-game winning-play))))
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
			  (<:submit :value "Search for another winning play"))))
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
	    (<:as-html player))
      (<:td :align "left"
	    (render statement))
      (<:td :align "left"
	    (if (initial-move? move)
		(<:em "(initial move)")
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
	     (if (initial-move? move)
		 (<:em "(initial move)")
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

(defun render-strategy (strategy)
  (let ((first-splitter (first-splitting-descendent strategy)))
    (if (null first-splitter)
	(let ((leaf (first (leaf-nodes strategy))))
	  (<:table
	   (render-segment-from-to-with-padding-as-row strategy leaf 0)))
	(let* ((succs (node-successors first-splitter))
	       (num-succs (length succs)))
	  (<:table :rules "groups"
		   :frame "void"
	   (<:thead
	    (render-segment-from-to-with-padding-as-row strategy first-splitter (floor (/ num-succs 2))))
	   (<:tbody
	    (<:tr
	     (if (evenp num-succs)
		 (progn
		   (loop
		      with cleft-point = (/ num-succs 2)
		      for i from 0 upto (1- cleft-point)
		      with succ = (nth i succs)
		      do
			(<:td :align "center"
			      (render-strategy succ)))
		   (<:td)
		   (loop
		      with cleft-point = (/ num-succs 2)
		      for i from cleft-point upto (1- num-succs)
		      with succ = (nth i succs)
		      do
			(<:td :align "center"
			      (render-strategy succ))))
		 (loop
		    for succ in succs
		    do
		      (<:td :align "center"
			    (render-strategy succ)))))))))))
		 
(defmethod render ((self winning-strategy-searcher))
  (with-slots (game depth play-style queue success)
      self
    (let ((result (winning-strategy (initial-statement game)
				    (dialogue-rules game)
				    depth
				    game)))
      (cond ((null result)
	     (<:h1 "Ouch!")
	     (<:p "Not only is there is no winning strategy that continues from the game above no more than " (<:as-html depth) " " (if (= depth 1) "move" "moves") ", there is actually " (<:em "no") " winning strategy at all that extends the initial game."))
	    ((eq result :dialogue-tree-too-shallow)
	     (<:h1 "Cutoff!")
	     (<:p "I couldn't find a winning strategy that extends the initial game at most " (<:as-html depth) " " (if (= depth 1) (<:as-is "move") (<:as-is "moves")) " beyond the end of the initial game.  The search was terminated because we reached the depth cutoff."))
	    (t ; something interesting
	     (let ((strat result))
	       (<:h1 "Success")
	       (<:p "Here is a continuation of the initial game for which Proponent has a winning strategy in no more than " (<:as-html depth) " " (if (= depth 1) (<:as-is "move") (<:as-is "moves")) " beyond the end of the initial game:")
	       (<:div :style "border:1px solid;"
	         (render-strategy strat))))))
    (<ucw:form :method "post"
	       :action (call 'turn-editor
			     :game game
			     :play-style play-style)
      (<:submit :value "Go back to the original game"))
    (<ucw:form :method "post"
	       :action (call 'start-game-component)
    (<:submit :value "Quit"))))

(defconstant max-search-depth 15
  "The maximum depth to which we permit searching for winning plays and winning strategies.")

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
    (<:div :style "border:1px solid;"
      (render-game game
		   :play-style play-style
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
  ;; (render-signature (dialogue-signature game))
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
		 " are open attacks.  (Defensive moves are not colored, nor is the initial move, since it is neither an attack nor a defense.) A dagger " (<:as-is "(&#8224;)") " in the Notes column indicates that alternative moves were available; follow the link to see them and rewind the game to explore an alternative course."))))))

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

(defmethod render ((sa (eql attack-left-disjunct)))
  (<:as-is "&or;")
  (<:sub "L"))

(defmethod render-plainly ((sa (eql attack-left-conjunct)))
  "&and;(L)")

(defmethod render-plainly ((sa (eql attack-left-disjunct)))
  "&or;(L)")

(defmethod render ((sa (eql attack-right-conjunct)))
  (<:as-is "&and;")
  (<:sub "R"))

(defmethod render ((sa (eql attack-right-disjunct)))
  (<:as-is "&or;")
  (<:sub "R"))

(defmethod render-plainly ((sa (eql attack-right-conjunct)))
  "&and;(R)")

(defmethod render-plainly ((sa (eql attack-right-disjunct)))
  "&or;(R)")

(defmethod render ((sa (eql which-instance?)))
  (<:as-is "?"))

(defmethod render-plainly ((sa (eql which-instance?)))
  "?")

(defmethod render ((sa (eql which-disjunct?)))
  (<:as-is "?"))

(defmethod render ((sa (eql which-conjunct?)))
  (<:as-is "?"))

(defmethod render-plainly ((sa (eql which-disjunct?)))
  "?")	   

(defmethod render-plainly ((sa (eql which-conjunct?)))
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

(defcomponent start-game-component (signature-component ruleset-component)
  ())

(defcomponent manual-formula-editor-component (signature-component)
  ())

(defmethod render ((self manual-formula-editor-component))
  (let ((input-formula nil)
	(sig (signature self)))
    (symbol-macrolet 
	(($formula
	  (let ((parsed-formula 
		 (ucw-handler-case (parse-formula input-formula)
		   (end-of-file () (call 'formula-corrector
					 :text input-formula
					 :signature sig))
		   (malformed-formula-error () (call 'formula-corrector
						     :text input-formula
						     :signature sig)))))
	    (if (belongs-to-signature? sig parsed-formula)
		(answer parsed-formula)
		(call 'formula-corrector
		      :text parsed-formula
		      :signature sig)))))
      (<:h1 "Enter a formula")
      ;; (<:p "The signature that you should use is:")
      ;; (<:blockquote
      ;; (render-signature (signature self)))
      (<ucw:form :method "POST"
		 :action $formula
		 (<ucw:input :type "text"
			     :accessor input-formula)
		 (<:submit :value "Use this formula"))
      (formula-guide))))

(defcomponent ruleset-info ()
  ())

(defmethod render ((self ruleset-info))
  (<:p (<:em (<:b "About the rules:")) " The rulesets in the
above menu are some notable cases that have some logical content.  You will be able to change your choice of ruleset once the game has started.  The names " (html-quote "D") " and " (html-quote "E") " come from W. Felscher's paper " (<:em "Dialogues, strategies, and intuitionistic provability") ", Annals of Pure and Applied Logic " (<:b "28") "(3), pp. 217" (<:as-is "&ndash;") "254, May 1985; it was arguably the first papers to rigorously establish the equivalence between intuitionistic validity and existence of winning strategies for certain dialogue games.  You will be able to alter your choice of rules after the game has begun."))

(defcomponent signature-info ()
  ())

(defmethod render ((self signature-info))
  (<:p (<:em (<:b "About the signature:")) "The signature is propositional, whose atoms are simply the letters of the alphabet: A, B, C, ... Z."))

(defcomponent translation-info ()
  ())

(defmethod render ((self translation-info))
  (<:p (<:em (<:b "About the translation:")) " The default is the
identity translation, so that whatever formula is chosen (or whatever
formula is entered into the text box) will be, verbatim, the formula
with which the game begins."))

(defcomponent formula-info ()
  ())

(defmethod render ((self formula-info))
  (formula-guide))

(defcomponent play-style-info ()
  ())

(defmethod render ((self play-style-info))
  (<:p (<:em (<:b "About the play style:")) " The default mode of playing is to take on the role of both players: at each move, you'll see all possible moves that can be made, from the perspective of both players.  Two other play styles are supported: play as Proponent with a random Opponent, and play as Opponent with a random Proponent."))

(defmethod render ((self start-game-component))
  (let (input-formula
	selected-formula 
	selected-rules
	selected-translation
	selected-play-style)
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
      (<ucw:form :method "post"
		 :action $take-action
      (<:table :style "border:1px solid;"
      (<:caption :style "caption-side:bottom;"
		 (<:submit :value "Let's play"))
       (<:tbody :style "border:1px solid;"
       (<:tr :style "background-color:#F063CD;"
	(<:td (<ucw:a :action (call 'formula-info)
		      "Formula:"))
	(<:td
	 (<ucw:select :id "selected-formula" 
		      :size 1 
		      :accessor selected-formula
	   (dolist (famous-formula (cons 't famous-formulas))
	     (if (eq famous-formula t)
		 (<ucw:option :value t
			      (<:as-is "(enter a formula manually)"))
		 (destructuring-bind (long-name short-name formula)
		     famous-formula
		   (declare (ignore short-name))
		   (<ucw:option :value formula (<:as-is long-name))))))))
       (<:tr :style "background-color:#A7007D;"
	(<:td (<ucw:a :action (call 'translation-info)
		      "Translation:"))
	(<:td (<ucw:select :id "selected-translation"
			   :size 1
			   :accessor selected-translation
	        (dolist (translation available-translations)
		  (<ucw:option :value translation
			       (<:as-is (description translation)))))))
       (<:tr :style "background-color:#7B942E;"
	(<:td (<ucw:a :action (call 'ruleset-info)
		      "Ruleset:"))
	(<:td (if (null (ruleset self))
		  (<ucw:select :id "selected-rules"
			       :size 1
			       :accessor selected-rules
			       (dolist (ruleset available-rulesets)
				 (<ucw:option :value ruleset
					      (<:as-html (description ruleset)))))
		  (<:as-html (description (ruleset self))))))
       (<:tr :style "background-color:#A3D800;"
         (<:td (<ucw:a :action (call 'play-style-info)
		       "Play style:"))
	 (<:td (<ucw:select :id "selected-play-style"
			    :size 1
			    :accessor selected-play-style
	         (<ucw:option :value 'play-as-both-proponent-and-opponent
			      "Play as both proponent and opponent")
		 (<ucw:option :value 'play-as-proponent-random-opponent
			      "Play as Proponent (Opponent will choose its moves randomly)")
		 (<ucw:option :value 'play-as-opponent-random-proponent
			      "Play as Opponent (Propnent will choose its moves randomly)")))))))))))

;;; ucw-site.lisp ends here