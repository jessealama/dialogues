;;; ucw-formulas UCW functionality for formulas, terms, signatures, and translations

(in-package :dialogues)

(defclass signature-component ()
  ((signature :initarg :signature
	      :initform *alphabetic-propositional-signature*
	      :type finite-variable-propositional-signature
	      :accessor signature)))

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

(defcomponent formula-corrector (signature-component)
  ((text :initarg :text :accessor formula-corrector-text)))

(defaction parse-formula-action (formula-str signature)
  (answer
   (ucw-handler-case
       (parse-formula formula-str)
     (malformed-formula-error ()
       (call 'formula-corrector
	     :text formula-str
	     :signature signature))
     (error ()
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
		    :size "160"
		    :accessor input-formula)
	(<:submit :value "Use this formula"))))

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
			     :size "160"
			     :accessor input-formula)
		 (<:submit :value "Use this formula"))
      (formula-guide))))

(defcomponent formula-info ()
  ())

(defmethod render ((self formula-info))
  (formula-guide))

(defparameter available-translations
  (list identity-translation
	goedel-gentzen-translation 
	double-negate-translation
	double-negate-all-subformulas-translation
	;; kuroda-translation
	negate-atomic-subformulas-translation
	double-negate-atomic-subformulas-translation
	self-conjoin-atomic-subformulas-translation
	self-disjoin-atomic-subformulas-translation
	contrapositivify-translation
	contrapositive-translation
	atomic->excluded-middle-translation
	converse-translation))

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
    ("I formula" "i-formula" ,i-formula)
    ("S formula" "s-formula" ,s-formula)
    ("Scott's formula" "scott-formula" ,scott-formula)
    ("Smetanich's formula" "smetanich-formula" ,smetanich-formula)
    ("&not;(P &and; Q) &rarr; (&not;P &or; &not;Q)" "de-morgan-not-and-implies-or-not" ,de-morgan-not-and-implies-or-not)
    ("&not;(P &or; Q) &rarr; (&not;P &and; &not;Q)" "de-morgan-not-or-implies-and-not" ,de-morgan-not-or-implies-and-not)
    ("(&not;P &and; &not;Q) &rarr; &not;(P &or; Q)" "de-morgan-and-not-implies-not-or" ,de-morgan-and-not-implies-not-or)
    ("(&not;P &or; &not;Q) &rarr; &not;(P &and; Q)" "de-morgan-or-not-implies-not-and" ,de-morgan-or-not-implies-not-and)
    ("(P &rarr; &not;P) &or; (&not;P &rarr; P)" "anti-connexive-formula" ,anti-connexive-formula)
    ("Ex contradictione quodlibet" "ex-contradictione-quodlibet" ,ex-contradictione-quodlibet)
    ("Implicational ex falso quodlibet" "implicational-ex-falso" ,implicational-ex-falso)
    ("KP" "kp" ,kp)
    ("WKP" "wkp" ,wkp)
    ("Distributivity of implication over disjunction" "distributivity-of-implication-over-disjunction" ,distributivity-of-implication-over-disjunction)
    ("Aristotle's thesis (positive antecedent)" "aristotles-thesis-positive-antecedent" ,aristotles-thesis-positive-antecedent)
    ("Aristotle's thesis (negative antecedent)" "aristotles-thesis-negative-antecedent" ,aristotles-thesis-negative-antecedent)
    ("Modus ponens" "modus-ponens" ,modus-ponens)
    ("Modus tollens" "modus-tollens" ,modus-tollens)
    ("Hypothetical syllogism" "hypothetical-syllogism" ,hypothetical-syllogism)
    ("Disjunctive syllogism" "disjunctive-syllogism" ,disjunctive-syllogism)
    ("Constructive dilemma" "constructive-dilemma" ,constructive-dilemma)
    ("Destructive dilemma" "destructive-dilemma" ,destructive-dilemma)
    ("Conjunction elimination (left)" "conjunction-elimination-left" ,conjunction-elimination-left)
    ("Conjunction elimination (right)" "conjunction-elimination-right" ,conjunction-elimination-right)
    ("Conjunction introduction" "conjunction-introduction" ,conjunction-introduction)
    ("Conjunction introduction" "conjunction-introduction" ,conjunction-introduction)
    ("Disjunction introduction (left)" "disjunction-introduction-left" ,disjunction-introduction-left)
    ("Disjunction introduction (right)" "disjunction-introduction-right" ,disjunction-introduction-right)
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
    ("Idempotency of disjunction (disjunctive consequent)" "disjunctive-idempotenency-disjunctive-consequent" ,disjunctive-idempotenency-disjunctive-consequent)
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
    ("McColl's Connexive Axiom 12" "connexive-ax-12" ,connexive-ax-12)

    ("Implicational disjunction introduction" "il-disjunction-formula" ,il-disjunction-formula)
    ("Implicational contraposition" "il-negation-formula" ,il-negation-formula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Groups of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *intuitionistic-logic-formulas*
  (list (find k-formula famous-formulas :key #'third)
	(find s-formula famous-formulas :key #'third)
	(find conjunction-introduction famous-formulas :key #'third)
	(find conjunction-elimination-left famous-formulas :key #'third)
	(find conjunction-elimination-right famous-formulas :key #'third)
	(find disjunction-introduction-left famous-formulas :key #'third)
	(find disjunction-introduction-right famous-formulas :key #'third)
	(find il-disjunction-formula famous-formulas :key #'third)
	(find il-negation-formula famous-formulas :key #'third)
	(find implicational-ex-falso famous-formulas :key #'third)))

(defparameter *type-theory-formulas*
  (list (find k-formula famous-formulas :key #'third)
	(find b-formula famous-formulas :key #'third)
	(find w-formula famous-formulas :key #'third)
	(find c-formula famous-formulas :key #'third)
	(find s-formula famous-formulas :key #'third)
	(find i-formula famous-formulas :key #'third)))

(defparameter *intermediate-logic*
  (list (find weak-excluded-middle famous-formulas :key #'third)
	(find dummett-formula famous-formulas :key #'third)
	(find conditional-excluded-middle famous-formulas :key #'third)
	(find scott-formula famous-formulas :key #'third)
	(find smetanich-formula famous-formulas :key #'third)))

(defparameter *connexive-logic*
  (list (find connexive-ax-1 famous-formulas :key #'third)
	(find connexive-ax-2 famous-formulas :key #'third)
	(find connexive-ax-3 famous-formulas :key #'third)
	(find connexive-ax-4 famous-formulas :key #'third)
	(find connexive-ax-5 famous-formulas :key #'third)
	(find connexive-ax-6 famous-formulas :key #'third)
	(find connexive-ax-7 famous-formulas :key #'third)
	(find connexive-ax-8 famous-formulas :key #'third)
	(find connexive-ax-9 famous-formulas :key #'third)
	(find connexive-ax-10 famous-formulas :key #'third)
	(find connexive-ax-11 famous-formulas :key #'third)
	(find connexive-ax-12 famous-formulas :key #'third)
	(find anti-connexive-formula famous-formulas :key #'third)))

(defparameter *de-morgan-rules*
  (list   (find de-morgan-not-and-implies-or-not famous-formulas :key #'third)
	  (find de-morgan-not-or-implies-and-not famous-formulas :key #'third)
	  (find de-morgan-and-not-implies-not-or famous-formulas :key #'third)
	  (find de-morgan-or-not-implies-not-and famous-formulas :key #'third)))

(defparameter *modal-logic*
  (list (find kp famous-formulas :key #'third)
	(find wkp famous-formulas :key #'third)))

(defparameter *syllogism-formulas*
  (list (find modus-ponens famous-formulas :key #'third)
	(find modus-tollens famous-formulas :key #'third)
	(find hypothetical-syllogism famous-formulas :key #'third)
	(find disjunctive-syllogism famous-formulas :key #'third)
	(find constructive-dilemma famous-formulas :key #'third)
	(find destructive-dilemma famous-formulas :key #'third)))

(defparameter *classical-logic-formulas*
  (list (find peirce-formula famous-formulas :key #'third)
	(find excluded-middle famous-formulas :key #'third)
	(find double-negation-elimination famous-formulas :key #'third)))

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

;;; ucw-formulas.lisp
