;;; ucw-ruleset.lisp UCW functionality for rulesets

(in-package :dialogues)

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

(defun render-heuristics (heuristic-list)
  (if (null heuristic-list)
      (<:em "(none)")
      (<:ul
       (dolist (heuristic heuristic-list)
	 (<:li
	  (<:strong (<:as-html (name heuristic)))
	  ": "
	  (<:as-html (description heuristic)))))))

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

(defparameter *available-rulesets*
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

;;; ucw-ruleset.lisp ends here