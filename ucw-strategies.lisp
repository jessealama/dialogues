;;; ucw-strategies UCW functionality around strategies

(in-package :dialogues)

(defcomponent strategy-editor (ruleset-component)
  ((strategy :accessor strategy
	     :initarg :strategy
	     :initform nil
	     :type (or null strategy))
   (player
    :accessor player
    :initarg :player
    :initform 'p
    :type symbol
    :documentation "The player for whom we are interactively looking for a strategy.  It should be other the symbol P or the symbol O.")
   (choice-node
    :accessor choice-node
    :initarg :choice-node
    :initform nil
    :type (or null strategy-node)
    :documentation "The Opponent node that has multiple Proponent responses that we are exploring")
   (current-choice
    :accessor current-choice
    :initarg :current-choice
    :initform nil
    :type (or null strategy-node)
    :documentation "The Proponent node that was most recently selected for exploration.")
   (winning-nodes
    :accessor winning-nodes
    :initarg :winning-nodes
    :initform nil
    :type list
    :documentation "The list of Proponent nodes that have led to a winning strategy.")
   (losing-nodes
    :accessor losing-nodes
    :initarg :losing-nodes
    :initform nil
    :type list
    :documentation "The list of Proponent nodes that have led to a loss (i.e., a failure to find a winning strategy).")
   (alternatives
    :accessor alternatives
    :initarg :alternatives
    :initform nil
    :type list
    :documentation "The list of alternatives (which are Proponent nodes) that are yet to be explored.")))

(defmethod render ((self strategy-editor))
  (let* ((strategy (strategy self))
	 (player (player self))
	 (heuristics (heuristics self))
	 (extra-rules (extra-rules self))
	 (player-choice (if (eql player 'p)
			    (first-proponent-choice strategy)
			    (first-opponent-choice strategy))))
    (<:table
     :rules "cols"
     :width "100%"
     :frame "box"
     :cellpadding "5"
     :bgcolor "IndianRed"
     (<:colgroup
      (<:col)
      (<:col)
      (<:col)
      (<:col)
      (<:col))
     (<:thead
      :style "border-bottom: solid 1px;"
      (<:tr
       (<:th "Ruleset")
       (<:th "Player")
       (<:th "Formula")
       (<:th "Extra Rules")
       (<:th "Heuristics")))
     (<:tbody
      (<:tr
       (<:td :nowrap "nowrap"
	     :align "center"
	     (<:strong (<:as-html (name (ruleset strategy))))
	     ": "
	     (<:as-html (description (ruleset strategy))))
       (<:td
	:align "center"
	(if (eql player 'p)
	    (<:strong "P")
	    (<:strong "O")))
       (<:td :nowrap "nowrap"
	     :align "center"
	     (<:as-is (render-fancily (move-statement (move (root strategy))))))
       (if extra-rules
	   (<:td
	    :align "left"
	    (render-heuristics extra-rules))
	   (<:td
	    :align "center"
	    (render-heuristics extra-rules)))
       (if heuristics
	   (<:td 
	    :align "left"
	    (render-heuristics heuristics))
	   (<:td 
	    :align "center"
	    (render-heuristics heuristics))))))
    (<:br) ;; no like
    (if (eq player-choice :too-deep)
	(<:p "I couldn't find the first choice node before I hit depth " (<:as-is +strategy-max-depth+) "; sorry, we can't play any more.  Please try some other formula or ruleset.")
	(progn
	  (if player-choice
	      (progn
		(render-strategy-with-alternative-hiding-closed-branches
		 (node->strategy player-choice (ruleset (strategy self)))
		 player-choice)
		(setf (choice-node self) player-choice))
	      (progn
		(if (eql player 'p)
		    (if (winning-strategy-for-proponent? strategy)
			(progn
			  (render-strategy-with-alternative strategy nil)
			  (<:p "Congratulations!  You've found a winning strategy for Proponent."))
			
			(progn
			  (<:p "I'm sorry to say that there is no winning strategy consistent with your choices so far.  (If you didn't make any choices at all, this means that the formula you started with,")
			  (<:blockquote
			   (render (move-statement (move (root strategy)))) ",")
			  (<:p "is invalid with respect to the ruleset that you chose.)")))
		    (if (winning-strategy-for-opponent? strategy)
			(progn
			  (<:p "Congratulations!  You've found a winning strategy for Opponent.  Here it is:")
			  (render-strategy-with-alternative strategy nil))
			
			(progn
			  (<:p "I'm sorry to say that there is no winning strategy for Opponent consistent with your choices so far.  (If you didn't make any choices at all, this means that the formula you started with,")
			  (<:blockquote
			   (render (move-statement (move (root strategy)))) ",")
			  (<:p "is valid with respect to the ruleset that you chose.)"))))))))
    
    (<:a :href "/" "[Quit]")))

(defcomponent winning-strategy-searcher (game-component play-style-component)
  ((depth :initarg :depth
	  :accessor depth)
   (queue :initarg :queue
	  :accessor queue
	  :initform nil)
   (success :initarg :success
	    :accessor success
	    :initform nil)))

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
	     (<:strong (<:as-html player)))
       (<:td :align "left"
	     :nowrap "nowrap"
	     (<:as-is (render-fancily statement)))
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

(defmacro ruleset-row (ruleset)
  (let ((id (format nil "~a-radio" ruleset)))
    `(<:tr
      :valign "top"
      (<:td
       :nowrap "nowrap"
       (<ucw:input :type "radio"
		   :name "selected-rules"
		   :accessor (name ,ruleset)
		   :value (name ,ruleset)
		   :id ,id)
       (<:label
	:for ,id
	(<:strong (<:as-html (name ,ruleset)))))
      (<:td
       (<:as-html (description ,ruleset))))))

(defmacro ruleset-option (ruleset)
  `(<ucw:option
    :value ',ruleset
    :title (description ,ruleset)
    (<:as-html (name ,ruleset) ": " (description ,ruleset))))

(defmacro rule-checkbox-row (rule selector)
  (let ((id (format nil "~a-checkbox" rule)))
    `(<:tr
      :valign "top"
      (<:td
       (<ucw:input
	:type "checkbox"
	:accessor ,selector
	:name (name ,rule)
	:value ',rule
	:id ,id)
       (<:label
	:for ,id
	(<:strong (<:as-html (name ,rule))))
       " "
       (<:as-html (description ,rule))))))

(defun move-as-colored-row (index move &key color)
  (with-slots (player statement stance reference)
      move
    (if color
	(<:tr
	 :nowrap "nowrap"
	 :valign "top"
	 :bgcolor color
	 :style "color:white;"
	 (<:td (<:as-html index))
	 (<:td (<:strong (<:as-html player)))
	 (<:td
	  :nowrap "nowrap"
	  (<:as-is (render-fancily statement)))
	 (unless (initial-move? move)
	   (if (attacking-move? move)
	       (<:td (<:as-html "[A," reference "]"))
	       (<:td (<:as-html "[D," reference "]")))))
	(<:tr
	 :nowrap "nowrap"
	 :valign "top"
	 (<:td (<:as-html index))
	 (<:td (<:strong (<:as-html player)))
	 (<:td
	  :nowrap "nowrap"
	  (<:as-is (render-fancily statement)))
	 (unless (initial-move? move)
	   (if (attacking-move? move)
	       (<:td (<:as-html "[A," reference "]"))
	       (<:td (<:as-html "[D," reference "]"))))))))

(defun render-strategy-node-as-table-row (node &optional alternatives)
  (let ((move (move node))
	(depth (depth node)))
    (with-slots (player statement stance reference)
	move
      (if (member node alternatives)
	  (<:tr
	   :bgcolor "indigo"
	   :nowrap "nowrap"
	   :valign "top"
	   :style "font-style:bold;color:white;"
	   (<:td
	    :nowrap "nowrap"
	    :align "center"
	    (<ucw:a
	     :action (setf (children (parent node))
			   (list node))
	     :style "text-decoration:none;color:white;"
	     :title (if (eq stance 'a)
			(format nil "Attack move #~d by asserting ~a" reference (render-plainly statement))
			(format nil "Defend against the attack of move #~d by asserting ~a" reference (render-plainly statement)))
	     (<:as-html depth)
	     " "
	     (<:strong (<:as-html player))
	     " "
	     (<:as-is (render-fancily statement))
	     " "
	     (unless (initial-move? move)
	       (if (attacking-move? move)
		   (<:as-html "[A," reference "]")
		   (<:as-html "[D," reference "]"))))))
	  (if (expanded? node)
	      (if (attacking-move? move)
		  (if (closed-in-every-branch? node depth)
		      (move-as-colored-row depth move :color "FireBrick")
		      (if (open-in-every-branch? node depth)
			  (move-as-colored-row depth move :color "ForestGreen")
			  (move-as-colored-row depth move) ))
		  (move-as-colored-row depth move))
	      (move-as-colored-row depth move :color "GoldenRod"))))))

(defun render-segment-with-padding-as-row (begin end padding &optional alternatives)
  "Given strategy nodes BEGIN and END, emit an HTML table
  row representing the dialogue from BEGIN to END.  The row will
  contain 2*PADDING + 1 columns; PADDING empty columns will be put on
  the left and the right of the sequence.  It is assumed that there is
  a path from BEGIN to END; the path is constrcted simply taking
  unique successors, starting at BEGIN, until we reach END.  The moves
  of the game between BEGIN and END will be put into a single HTML
  table element."
  (let ((sorted-alternatives (sort (copy-list alternatives)
				   #'move-< :key #'move)))
    (symbol-macrolet
	(($padding (dotimes (i padding) (<:td))))
      (<:tr 
       :valign "top"
       $padding
       (<:td
	:align "center"
	(<:table
	 :bgcolor "silver"
	 :cellspacing "0"
	 :style "align: center;"
	 (loop
	    for current-node = begin then (first (children current-node))
	    do
	      (render-strategy-node-as-table-row current-node
						 sorted-alternatives)
	      (when (eq current-node end)
		(return)))))
       $padding))))

(defun render-node-with-alternative-hiding-closed-branches (node alternative ruleset)
  (if (branch-closed? node)
      (<:table
       :style "align: center;"
       :bgcolor "silver"
       :cellspacing "0"
       (render-strategy-node-as-table-row node nil)
       (<:tr
	(<:td
	 :align "center"
	 :colspan "4"
	 :title "Wouldn't it be great if this were a link which, when followed, showed the subtree rooted at this node?"
	 (<:b (<:as-is "&hellip;"))))
       (<:tr
	(if (proponent-wins-every-branch? node ruleset)
	    (<:td
	     :align "center"
	     :colspan "4"
	     :title "Proponent wins every dialogue passing through here"
	     (<:span
	      :style "font-size:xx-large;"
	      (<:as-is "&#9786")))
	    (if (opponent-wins-every-branch? node ruleset)
		(<:td
		 :align "center"
		 :colspan "4"
		 :title "Opponent wins every dialogue passing through here"
		 (<:span
		  :style "font-size:xx-large;"
		  (<:as-is "&#9785;")))
		(<:td
		 :align "center"
		 :colspan "4"
		 :title "Proponent wins at least one dialogue passing through here, and Opponents wins at least one dialogue passing through here"
		 (<:span
		  :style "font-size:xx-large;"
		  (<:as-is "&#9786; &#9785;")))))))
      (let ((first-splitter (first-splitter node)))
	(if (null first-splitter)
	    (let ((leaf (first (leaves node))))
	      (<:table
	       :style "align: center;"
	       :bgcolor "silver"
	       (render-segment-with-padding-as-row node
						   leaf
						   0
						   (when alternative
						     (children alternative)))))
	    (let* ((succs (children first-splitter))
		   (succs-sorted (sort (copy-list succs)
				       #'move-<
				       :key #'move))
		   (num-succs (length succs)))
	      (<:table :rules "groups"
		       :frame "void"
		       :bgcolor "silver"
		       :style "align: center;"
		       (<:thead
			(render-segment-with-padding-as-row node
							    first-splitter
							    (floor (/ num-succs 2))
						 (when alternative
						   (children alternative))))
	    (<:tbody
	     (<:tr :valign "top"
	      (if (evenp num-succs)
		  (progn
		    (loop
		       with cleft-point = (/ num-succs 2)
		       for i from 0 upto (1- cleft-point)
		       for succ = (nth i succs-sorted)
		       do
			 (<:td :align "center"
			   (render-node-with-alternative-hiding-closed-branches succ
										alternative
										ruleset)))
		    (<:td)
		    (loop
		       with cleft-point = (/ num-succs 2)
		       for i from cleft-point upto (1- num-succs)
		       for succ = (nth i succs-sorted)
		       do
			 (<:td :align "center"
			   (render-node-with-alternative-hiding-closed-branches succ
										alternative
										ruleset))))
		  (loop
		     for succ in succs-sorted
		     do
		       (<:td :align "center"
		         (render-node-with-alternative-hiding-closed-branches succ
									      alternative
									      ruleset))))))))))))

(defun render-node-with-alternative (node alternative)
  (let ((first-splitter (first-splitter node)))
    (if (null first-splitter)
	(let ((leaf (first (leaves node))))
	  (<:table
	   :style "align: center;"
	   :bgcolor "silver"
	   (render-segment-with-padding-as-row node
					       leaf
					       0
					       (when alternative
						 (children alternative)))))
	(let* ((succs (children first-splitter))
	       (succs-sorted (sort (copy-list succs)
				   #'move-<
				   :key #'move))
	       (num-succs (length succs)))
	  (<:table :rules "groups"
		   :frame "void"
		   :bgcolor "silver"
		   :style "align: center;"
	    (<:thead
	     (render-segment-with-padding-as-row node
						 first-splitter
						 (floor (/ num-succs 2))
						 (when alternative
						   (children alternative))))
	    (<:tbody
	     (<:tr :valign "top"
	      (if (evenp num-succs)
		  (progn
		    (loop
		       with cleft-point = (/ num-succs 2)
		       for i from 0 upto (1- cleft-point)
		       for succ = (nth i succs-sorted)
		       do
			 (<:td :align "center"
			   (render-node-with-alternative succ
							 alternative)))
		    (<:td)
		    (loop
		       with cleft-point = (/ num-succs 2)
		       for i from cleft-point upto (1- num-succs)
		       for succ = (nth i succs-sorted)
		       do
			 (<:td :align "center"
			   (render-node-with-alternative succ
							 alternative))))
		  (loop
		     for succ in succs-sorted
		     do
		       (<:td :align "center"
		         (render-node-with-alternative succ
						       alternative)))))))))))

(defun render-strategy-with-alternative-hiding-closed-branches (strategy alternative)
  "Render STRATEGY, with the children of strategy node ALTERNATIVE in
  a distinctive color, and link the the action of extending STRATEGY by setting the unique child of ALTERNATIVE to the indicated child.  Omit showing closed branches"
  (<:table
   :width "100%"
   :style "align: center;background-color:silver;"
   :frame "box"
   :summary "The strategy so far."
   (<:caption
    :style "background-color:silver;font-style:oblique;"
    :align "bottom"
    "The strategy so far.  Nodes in "
    (<:span
     :style "background-color:FireBrick;color:white;"
     "red")
    " are attacks that are closed in every branch passing through the node.  Nodes in "
    (<:span
     :style "background-color:ForestGreen;color:white;"
     "green")
    " are attacks that are open in every branch passing through the node.  "
    "Nodes in "
    (<:span
     :style "background-color:Indigo;color:white;"
     "purple")
    " indicate choices to be made.  Nodes in "
    (<:span
     :style "background-color:Goldenrod;color:white;"
     "gold")
    " are yet to be explored.")
   (<:tr
    (<:td
     :align "center"
     (render-node-with-alternative-hiding-closed-branches (root strategy) alternative (ruleset strategy))))))

(defun render-strategy-with-alternative (strategy alternative)
  "Render STRATEGY, with the children of strategy node ALTERNATIVE in
  a distinctive color, and link the the action of extending STRATEGY by setting the unique child of ALTERNATIVE to the indicated child."
  (<:table
   :width "100%"
   :style "align:center;background-color:silver;"
   :frame "box"
   :summary "The strategy so far."
   (<:caption
    :style "background-color:silver;font-style:oblique;"
    :align "bottom"
    "The strategy so far.  Nodes in "
    (<:span
     :style "background-color:FireBrick;color:white;"
     "red")
    " are attacks that are closed in every branch passing through the node.  Nodes in "
    (<:span
     :style "background-color:ForestGreen;color:white;"
     "green")
    " are attacks that are open in every branch passing through the node.  "
    "Nodes in "
    (<:span
     :style "background-color:Indigo;color:white;"
     "purple")
    " indicate choices to be made.")
   (<:tr
    (<:td
     :align "center"
     (render-node-with-alternative (root strategy) alternative)))))

;;; ucw-strategies.lisp ends here
