;;; strategy.lisp Working with dialogue strategies

(in-package :dialogues)

(defclass strategy-node ()
  ((move
    :type move
    :accessor move
    :initarg :move
    :documentation "The move taken at this node")
   (parent
    :type (or null strategy-node)
    :accessor parent
    :initarg :parent
    :initform nil
    :documentation "The parent of this node")
   (children
    :type list
    :accessor children
    :initarg :children
    :initform nil
    :documentation "The children of this node")
   (expanded
    :type boolean
    :accessor expanded?
    :initform nil
    :initarg :expanded
    :documentation "Whether we have expanded this node (i.e., computed the set of all possible children of this node, relative to the ruleset by which we are playing)."))
  (:documentation "A node in a strategy tree.  These trees need not be
  bona fide stratategies in the game-theoretic sense of the term: some
  of their branches might not be fully expanded; some
  branches might lead to losses; etc.  Strictly speaking, we are going
  to be considering only certain kinds of subtrees of ful dialogue
  trees, among which winning strategies can be found."))

(defun node->dialogue (strategy-node ruleset)
  "By following the edges from a strategy node to its parent until we
  reach a root, strategy nodes can be considered the end of a unique
  dialogue game.  Since strategy nodes do not \"know\" what the ruleset is according to which their corresponding move was made, whereas dialogues do contain this information, we pass a ruleset in as an argument."
  (with-slots (parent move)
      strategy-node
    (if parent
	(add-move-to-dialogue (dialogue parent) move)
	(make-dialogue (move-statement move)
		       alphabetic-propositional-signature ;; ugh
		       ruleset))))

(defun expand-strategy-node (node ruleset)
  (let* ((dialogue (node->dialogue node ruleset))
	 (move (move node))
	 (next-moves (if (proponent-move? move)
			 (next-opponent-moves dialogue)
			 (next-proponent-moves dialogue))))
    (setf (children node)
	  (mapcar #'(lambda (move)
		      (make-instance 'strategy-node
				     :move move
				     :parent node))
		  next-moves)))
  (setf (expanded? node) t)
  node)

(defun winning-node? (node ruleset)
  "A strategy node is called winning with respect to a ruleset if the
dialogue it represents, with respect to that ruleset, is won by
Proponent."
  (proponent-wins? (node->dialogue node ruleset)))

(defclass strategy ()
  ((root
    :type strategy-node
    :initarg :root
    :accessor root
    :documentation "The root node of the strategy (tree) under consideration.")
   (ruleset
    :type ruleset
    :initarg :ruleset
    :accessor ruleset
    :documentation "The ruleset by which this strategy is intended to adhere.")))

(defun nodes (strategy)
  "A list of all nodes of STRATEGY reachable from its root."
  (labels ((all-nodes (node)
	     (with-slots (children)
		 node
	       (if children
		   (cons node
			 (append children
				 (reduce #'append
					 (mapcar #'all-nodes children))))
		   (list node)))))
    (all-nodes (root strategy))))

(defun fully-expanded? (strategy)
  (with-slots (root)
      strategy
    (every #'node-expanded? (nodes strategy))))

(defun leaves (root)
  "Leaf nodes reachable from ROOT"
  (with-slots (children)
      root
    (if (null children)
	(list root)
	(reduce #'append (mapcar #'leaves children)))))

(defun winning-strategy-form? (strategy)
  "Determine whether STRATEGY has the graph-theoretic structure that a winning strategy has: (1) every node of odd depth (start counting at 0) has exactly one child"
  (labels ((even-node-ok? (node)
	     (with-slots (children)
		 node
	       (or (null children)
		   (every #'odd-node-ok? children))))
	   (odd-node-ok? (node)
	     (with-slots (children)
		 node
	       (and (length= 1 children)
		    (even-node-ok? (first children))))))
    (even-node-ok? (root strategy))))

(defun winning-strategy? (strategy)
  "Determine whether STRATEGY is a winning stratgy (for Proponent).  There are three conditions to satisfy: (1) for every Proponent node of the strategy, the set of its children is equal (in the set of being the same set of moves) to the set of all possible moves for Opponent at that node, and (2) every Opponnent node has exactly one child, (3) every leaf of the tree, considered as a dialogue, is a win for Proponent."
  (with-slots (root ruleset)
      strategy
    (and (winning-strategy-form? strategy)
	 (fully-expanded? strategy)
	 (every #'proponent-wins?
		(mapcar #'(lambda (leaf)
			    (node->dialogue leaf ruleset))
			(leaves root))))))

(defun first-proponent-choice (strategy)
  "The shallowest, leftmost Opponent node of STRATEGY where Proponent
has more than one possible move.  This function expands the nodes of
STRATEGY to find such a node; the expansion will use the ruleset of
the strategy.  If there no such node, return NIL."
  (first-proponent-choice-wrt-ruleset (root strategy)
				      (ruleset strategy)))

(defun first-proponent-choice-wrt-ruleset (node ruleset)
  (unless (expanded? node)
    (expand-strategy-node node ruleset))
  (let ((children (children node)))
    (when children
      (if (proponent-node? node)
	  (some #'(lambda (node)
		    (first-proponent-choice-wrt-ruleset node ruleset))
		children)
	  (if (cdr children)
	      node
	      (first-proponent-choice-wrt-ruleset (first children) ruleset))))))

(defclass strategy-with-choices (strategy)
  ((choices
    :type list
    :initform nil
    :initarg :choices
    :accessor choices
    :documentation "The list of choices that are available for this strategy.  It is intended to be a list of STRATEGY-NODE objects."))
  (:documentation "A STRATEGY-WITH-CHOICES is a strategy that keeps tracks of possible choices that were made as the strategy was expanded."))

;;; strategy.lisp ends here