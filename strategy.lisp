;;; strategy.lisp Working with dialogue strategies

(in-package :dialogues)

(defclass strategy-node ()
  ((move
    :type move
    :accessor move
    :initarg :move
    :documentation "The move taken at this node")
   (ruleset
    :type ruleset
    :accessor ruleset
    :initarg :ruleset
    :initform (error "A strategy node requires a ruleset to be present."))
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
    :documentation "The children of this node"))
  (:documentation "A node in a strategy tree.  These trees need not be bona fide stratategies in the game-theoretic sense of the term: some of their branches might not be fully expanded; some branches might lead to losses; etc.  Strictly speaking, we are going to be considering only certain kinds of subtrees of ful dialogue trees, among which winning strategies can be found."))

(defmethod expanded-p ((s strategy-node))
  "Whether we have expanded STRATEGY-NODE (that is, computed the set of all possible children of this node, relative to the ruleset by which we are playing)."
  (slot-boundp s 'children))

(defmethod print-object ((node strategy-node) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (children move)
	node
      (format stream "~a (with ~d children; ~:[unexpanded~;expanded~])"
	      move (length children) (expanded-p node)))))

(defun node->dialogue (strategy-node ruleset)
  "By following the edges from a strategy node to its parent until we
  reach a root, strategy nodes can be considered the end of a unique
  dialogue game.  Since strategy nodes do not \"know\" what the ruleset is according to which their corresponding move was made, whereas dialogues do contain this information, we pass a ruleset in as an argument."
  (with-slots (parent move)
      strategy-node
    (if parent
	(add-move-to-dialogue (node->dialogue parent ruleset) move)
	(make-instance 'dialogue
                       :initial-formulua (statement move)))))

(defun node-reference-< (node-1 node-2)
  (< (reference (move node-1))
     (reference (move node-2))))

(defun node-reference-> (node-1 node-2)
  (> (reference (move node-1))
     (reference (move node-2))))

(defun expand-strategy-node (node)
  (let* ((dialogue (node->dialogue node (ruleset node)))
	 (move (move node))
	 (next-moves (if (proponent-move-p move)
			 (next-opponent-moves dialogue)
			 (next-proponent-moves dialogue))))
    (setf (children node)
	  (mapcar #'(lambda (move)
		      (make-instance 'strategy-node
				     :move move
				     :parent node))
		  next-moves)))
  node)

(defun winning-node? (node ruleset)
  "A strategy node NODE is called winning with respect to ruleset RULESET if the dialogue it represents, with respect to that ruleset, is won by Proponent."
  (proponent-wins-p (node->dialogue node ruleset)))

(defgeneric first-splitter (thing)
  (:documentation "Find the shallowest node of THING that has multiple children."))

(defmethod first-splitter ((node strategy-node))
  (let ((children (children node)))
    (cond ((null children) nil)
	  ((null (cdr children)) (first-splitter (first children)))
	  (t node))))

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

(defmethod print-object ((strategy strategy) stream)
  (labels ((print-node (node)
	     (with-slots (move children)
		 node
	       (with-slots (player statement stance reference)
		   move
		 (if (and stance reference)
		     (format stream "({~a ~a [~a,~d]} " player statement stance reference)
		     (format stream "({~a ~a (initial move)} " player statement))
		 (dolist (child children)
		   (print-node child))
		 (format stream ")")))))
    (print-unreadable-object (strategy stream :type t)
      (print-node (root strategy)))))

(defun nodes (strategy)
  "A list of all nodes of STRATEGY reachable from its root."
  (labels ((all-nodes (node)
	     (with-slots (children)
		 node
	       (if children
		   (cons node
			 (reduce #'append
				 (mapcar #'all-nodes children)))
		   (list node)))))
    (all-nodes (root strategy))))

(defun node->strategy (node ruleset)
  "From NODE determine a strategy (with respect to RULESET) by finding
  the root from which NODE comes"
  (labels ((find-root (n)
	     (if (null (parent n))
		 n
		 (find-root (parent n)))))
    (make-instance 'strategy
		   :root (find-root node)
		   :ruleset ruleset)))

(defun fully-expanded? (strategy)
  (every #'expanded-p (nodes strategy)))

(defgeneric leaves (thing)
  (:documentation "Leaf nodes reachable from ROOT"))

(defmethod leaves :before ((node strategy-node))
  (unless (expanded-p node)
    (expand-strategy-node node)))

(defmethod leaves ((node strategy-node))
  (with-slots (children)
      node
    (if (null children)
        (list node)
        (reduce #'append (mapcar #'leaves children)))))

(defmethod leaves ((strategy strategy))
  (leaves (root strategy)))

(defun winning-strategy-for-proponent-form? (strategy)
  "Determine whether STRATEGY has the graph-theoretic structure that a winning strategy for Player 1 has: (1) every node of odd depth (start counting at 0) has exactly one child"
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

(defun winning-strategy-for-opponent-form? (strategy)
  "Determine whether STRATEGY has the graph-theoretic structure that a winning strategy for Player 2 has: (1) every node of even depth (start counting at 0) has exactly one child"
  (labels ((even-node-ok? (node)
	     (with-slots (children)
		 node
	       (and (length= 1 children)
		    (odd-node-ok? (first children)))))
	   (odd-node-ok? (node)
	     (with-slots (children)
		 node
	       (or (null children)
		   (every #'even-node-ok? children)))))
    (even-node-ok? (root strategy))))

(defun winning-strategy-for-proponent? (strategy)
  "Determine whether STRATEGY is a winning stratgy for Proponent.  There are three conditions to satisfy: (1) for every Proponent node of the strategy, the set of its children is equal (in the set of being the same set of moves) to the set of all possible moves for Opponent at that node, and (2) every Opponnent node has exactly one child, (3) every leaf of the tree, considered as a dialogue, is a win for Proponent."
  (when (winning-strategy-for-proponent-form? strategy)
    (when (fully-expanded? strategy)
      (with-slots (root ruleset)
          strategy
        (every #'proponent-wins-p
                    (mapcar #'(lambda (leaf)
                                (node->dialogue leaf ruleset))
                            (leaves root)))))))

(defun winning-strategy-for-opponent? (strategy)
  "Determine whether STRATEGY is a winning stratgy for Opponent.  There are three conditions to satisfy: (1) for every Opponent node of the strategy, the set of its children is equal (in the set of being the same set of moves) to the set of all possible moves for Proponent at that node, and (2) every Proponent node has exactly one child, (3) every leaf of the tree, considered as a dialogue, is a win for Opponent."
  (when (winning-strategy-for-opponent-form? strategy)
    (when (fully-expanded? strategy)
      (with-slots (root ruleset)
          strategy
        (every #'opponent-wins-p
               (mapcar #'(lambda (leaf)
                           (node->dialogue leaf ruleset))
                       (leaves root)))))))

(defun first-proponent-choice (strategy)
  "The shallowest, leftmost Opponent node of STRATEGY where Proponent
has more than one possible move.  This function expands the nodes of
STRATEGY to find such a node; the expansion will use the ruleset of
the strategy.  If there no such node, return NIL."
  (first-proponent-choice-wrt-ruleset (root strategy)
				      (ruleset strategy)))

(defun first-opponent-choice (strategy)
  "The shallowest, leftmost Proponent node of STRATEGY where Opponent
has more than one possible move.  This function expands the nodes of
STRATEGY to find such a node; the expansion will use the ruleset of
the strategy.  If there no such node, return NIL."
  (first-opponent-choice-wrt-ruleset (root strategy)
				     (ruleset strategy)))

(defgeneric proponent-node? (node))
(defgeneric opponent-node? (node))

(defmethod proponent-node? ((node strategy-node))
  (proponent-move-p (move node)))

(defmethod opponent-node? ((node strategy-node))
  (opponent-move-p (move node)))

(defmethod depth ((node strategy-node))
  (let ((p (parent node)))
    (if (null p)
	0
	(1+ (depth p)))))

(define-constant +strategy-max-depth+
    75
  :test #'=
  :documentation "The maximum depth to which we will develop a strategy node.")

(defun first-proponent-choice-wrt-ruleset (node ruleset &optional (max-depth +strategy-max-depth+))
  (unless (expanded-p node)
    (expand-strategy-node node))
  (let ((d (depth node)))
    (if (< d max-depth)
	(let ((children (children node)))
	  (when children
	    (if (proponent-node? node)
		(some #'(lambda (node)
			  (first-proponent-choice-wrt-ruleset node ruleset max-depth))
		      children)
		(if (cdr children)
		    node
		    (first-proponent-choice-wrt-ruleset (first children) ruleset max-depth)))))
	:too-deep)))

(defun first-opponent-choice-wrt-ruleset (node ruleset &optional (max-depth +strategy-max-depth+))
  (unless (expanded-p node)
    (expand-strategy-node node))
  (let ((d (depth node)))
    (if (< d max-depth)
	(let ((children (children node)))
	  (when children
	    (if (opponent-node? node)
		(some #'(lambda (node)
			  (first-opponent-choice-wrt-ruleset node
							     ruleset
							     max-depth))
		      children)
		(if (cdr children)
		    node
		    (first-opponent-choice-wrt-ruleset (first children) ruleset max-depth)))))
	:too-deep)))

(defclass strategy-with-choices (strategy)
  ((choices
    :type list
    :initform nil
    :initarg :choices
    :accessor choices
    :documentation "The list of choices that are available for this strategy.  It is intended to be a list of STRATEGY-NODE objects."))
  (:documentation "A STRATEGY-WITH-CHOICES is a strategy that keeps tracks of possible choices that were made as the strategy was expanded."))

(defmethod first-splitter ((strategy strategy))
  (first-splitter (root strategy)))

(defun every-branch (node predicate)
  "Determine whether PREDICATE holds of every brach of tree that
  passes through node NODE.  PREDICATE is applied the each leaf node reachable from NODE."
  (every predicate (leaves node)))

(defun segment (from to)
  "A list of strategy nodes starting from FROM and ending at TO.
  It is assumed that FROM and TO are coming from the same tree, and
  that there is in fact a path between them."
  (loop
     with path = (list from)
     for node = to then (parent node)
     do
       (if (eq node from)
	   (return (reverse path))
	   (push node path))))

(defun branch-closed? (node)
  "Determine whether NODE represents a closed branch: every leaf
  reachable from NODE is expanded."
  (when (expanded-p node)
    (with-slots (children)
        node
      (or (null children)
          (every #'branch-closed? children)))))

(defun proponent-wins-every-branch? (node ruleset)
  "Determine whether Proponent wins every dialogue that passes through NODE."
  (every #'proponent-wins-p
	 (mapcar #'(lambda (leaf)
		     (node->dialogue leaf ruleset))
		 (leaves node))))

(defun opponent-wins-every-branch? (node ruleset)
  "Determine whether Proponent wins every dialogue that passes through NODE."
  (every #'opponent-wins-p
	 (mapcar #'(lambda (leaf)
		     (node->dialogue leaf ruleset))
		 (leaves node))))

(defun closed-in-every-branch? (node attack-index)
  "Determine whether ATTACK-INDEX, which is assumed to be the
  index (starting from 0) of an attack in the dialogue tree implicitly
  represented by NODE, is closed in every branch of the tree passing
  through NODE."
  (flet ((branch-closed? (leaf)
	   (find-if #'(lambda (move)
			(and (defense-p move)
			     (= (reference move) attack-index)))
		    (segment node leaf)
		    :key #'move)))
    (every-branch node #'branch-closed?)))

(defun open-in-every-branch? (node attack-index)
  "Determine whether ATTACK-INDEX, which is assumed to be the
  index (starting from 0) of an attack in the dialogue tree implicitly
  represented by NODE, is open in every branch of the tree passing
  through NODE."
  (flet ((branch-open? (leaf)
	   (not
	    (find-if #'(lambda (move)
			 (and (defense-p move)
			      (= (reference move) attack-index)))
		     (segment node leaf)
		     :key #'move))))
    (every-branch node #'branch-open?)))

(defmethod attack-p ((s strategy-node))
  (attack-p (move s)))

(defmethod defense-p ((s strategy-node))
  (defense-p (move s)))

;;; strategy.lisp ends here
