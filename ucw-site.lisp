;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogues)

(defentry-point "" (:application *dialogue-application*)
    ()
  (call 'initial-formula-window))

(defentry-point "/game"
    (:application *dialogue-application*)
    ((signature)
     (initial-formula)))

(defentry-point "/strategy"
    (:application *dialogue-application*)
    ((initial-formula)
     (base-ruleset)
     (extra-rules)
     (heuristic-rules)))

(defentry-point "/about"
    (:application *dialogue-application*)
    (call 'about-this-site-component))

(defcomponent start-game-component (signature-component ruleset-component)
  ())

(defmethod render ((self start-game-component))
  (let (selected-formula
	selected-play-style
	selected-translation
	selected-rules
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
	rule-no-repetitions-checked
	opponent-no-repeats-checked
	proponent-no-repeats-checked)
    (with-slots ((sig signature))
      self
    (symbol-macrolet
	(($formula
	  (if (eq selected-formula t)
	      (call 'manual-formula-editor-component
		    :signature sig)
	      (if (belongs-to-signature? sig selected-formula)
		  selected-formula
		  (call 'formula-corrector
			:text selected-formula
			:signature sig))))
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
	  (<:td
	   :align "center"
	   :style "font-weight:bold;"
	   :title "Select an initial formula, or choose to enter your own formula manually."
	   "Formula")
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
	  (<:td
	   :align "center"
	   :style "font-weight:bold;"
	   :title "Choose a way of modifying the formula that you've
selected. The default is the identity translation, which does nothing."
	   "Translation")
	  (<:td (<ucw:select
		 :id "selected-translation"
		 :size 1
		 :accessor selected-translation
		 (dolist (translation available-translations)
		   (<ucw:option
		    :value translation
		    (<:as-is (description translation)))))))
	 (<:tr
	  :style "background-color:lightcoral;"
	  (<:td
	   :align "center"
	   :style "font-weight:bold;"
	   :title "Choose from one of the predefined well-established rulesets.  Its rules will be included in the ruleset according to which your game will be played.  By default, only a skeletal ruleset (no structural rules) will be used."
	   "Ruleset")
	  (<:td
	   :title "Choose from one of the predefined well-established rulesets.  Its rules will be included in the ruleset according to which your game will be played.  By default, only a skeletal ruleset (no structural rules) will be used."
	   (<ucw:select
	    :accessor selected-rules
	    :title "The list of base rulesets is taken from the literature on dialogue games (see, for instance, W. Felscher's 'Dialogues, strategies, and intuitionistic provability', Annals of Pure and Applied Logic 28(3), pp. 217-254).  (The names 'D' and 'E' and the names of the standard structural rules come from this paper.)"
	    (ruleset-option skeletal-rules)
	    (ruleset-option d-dialogue-rules)
	    (ruleset-option e-dialogue-rules)
	    (ruleset-option classical-dialogue-rules)
	    (ruleset-option nearly-classical-dialogue-rules))))
	 (<:tr
	  :style "background-color:#7B942E;"
	  (<:td
	   :align "center"
	   :style "font-weight:bold;"
	   :title "After choosing a base ruleset, you may optionally select other rules. The extra rules come in three kinds: standard structural rules, experimental, non-standard structural rules, and heuristics.  Strictly speaking, there is no difference between these different kinds of rules; they are all structural rules on a par with one another.

The standard structural rules are commonly used in the literature on dialogue games (Felscher, Krabbe, etc.).

The experimental rules are just that: experiments.  They are definitely non-standard, and sometimes even whimsical.

The list of heuristic rules can be used to help cut down the set of possibilities.  For example, one might be interested in games where neither player can repeat and earlier move.  (But note that these heuristic rules, since they are on a par with all the other rules, can have significant logical impact.  You have been warned.)

The ruleset that will be used during the game will be the union of the rules in the chosen base rule set, together with whatever optional, extra rules were chosen.  We have a adopted a 'the-user-is-always-right' approach: there is no check for whether the constructed ruleset is 'consistent' or has any logical significance."
		"Extras")
	  (<:td (if (null (ruleset self))
		    (<:table
		     :rules "cols"
		     :summary "The purpose of this table is to augment the ruleset that was chosen in the previous file."
		     (<:thead
		      (<:colgroup
		       (<:col)
		       (<:col)
		       (<:col))
		      (<:tr
		       (<:th
			:title "Choose whether to include some of the standard rules taken from the literature on dialogue games.  Some of the standard rulesets already include these rules; if you choose a rule here that is already included in the ruleset you've chosen, you choice will be ignored."
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
		       :valign "center"
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
	  (<:td
	   :align "center"
	   :style "font-weight:bold;"
	   :title "Choose how you want to play a game.  You can choose to play a concrete dialogue as either the Proponent or Opponent, or as both.  At each stage of the game, you will see all possible ch move, you'll see all possible moves that can be made.  You can also choose to interactively find a winning strategy (and you can choose for which player you want to find a strategy)."
	   "Play Style")
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

(defcomponent initial-formula-window (standard-window-component)
  ()
  (:default-initargs
      :title "explore dialogical logic with lorenzen dialogue games"
    :doctype yaclml:+xhtml-strict-doctype+
    :body (make-instance 'start-game-component)))

;;; ucw-site.lisp ends here