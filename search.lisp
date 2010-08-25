;;; search.lisp: Problems, nodes, search trees, search strategies

(in-package :dialogues)

(defstruct problem
  "A problem is defined by the initial state, and the type of problem it is.
We will be defining subtypes of PROBLEM later on.  For bookkeeping, we
count the number of nodes expanded."
  (initial-state)  ; A state in the domain.
  (goal)           ; Optionally store the desired state here.
  (num-expanded 0) ; Number of nodes expanded in search for solution.
  )

(defstruct node
  "Node for generic search.  A node contains a state, a
domain-specific representation of a point in the search space.  It
also contains some bookkeeping information."
  (state)                   ; a state in the domain
  (parent nil)              ; the parent node of this node
  (action nil)              ; the domain action leading to state
  (successors nil)          ; list of successor nodes
  (depth 0)                 ; depth of node in tree (root = 0)
  (expanded? nil)           ; any successors examined?
  )



(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (state)
	node
      (format stream "~A" state))))

(defmethod successors ((problem problem) node)
  "Return an alist of (action . state) pairs, reachable from this state."
  (declare (ignore node))
  (error "You need to define a SUCCESSORS method for ~A" problem))

(defmethod goal-test ((problem problem) node)
  "Return true or false: is this a goal node?  This default method
checks if the state is equal to the state stored in the problem-goal
slot.  You will need to define your own method if there are multiple
goals, or if you need to compare them with something other than
EQUAL."
  (equal (node-state node) (problem-goal problem)))

(defun node-ancestors (node)
  "The ancestors of NODE, starting with its most distant
ancestor (i.e., the ancestor of NODE whose parent is NIL)."
  (labels ((node-ancestors-backwards (n)
	     (if (node-parent n)
		 (cons n (node-ancestors-backwards (node-parent n)))
		 (list n))))
    (reverse (node-ancestors-backwards (node-parent node)))))

(defun always-zero (n a s)
  (declare (ignore n a s))
  0)

(defun always-one (n a s)
  (declare (ignore n a s))
  1)

(defun expand (node problem)
  (if (node-expanded? node)
      (node-successors node)
      (progn
	(setf (node-expanded? node) t)
	(incf (problem-num-expanded problem))
	(loop 
	   with nodes = nil
	   for successor in (successors problem node)
	   do
	     (destructuring-bind (action . state)
		 successor
	       (push (make-node :parent node 
				:action action 
				:state state
				:depth (1+ (node-depth node)))
		     nodes))
	   finally
	     (setf (node-successors node) nodes)
	     (return nodes)))))

(defun create-start-node (problem)
  "Make the starting node, corresponding to the problem's initial state."
  (make-node :state (problem-initial-state problem)))

(defun leaf-nodes (node)
  "All nodes reachable from NODE (via the successor function) that are either unexpanded or have no successors (and are expanded)."
  (if (node-expanded? node)
      (let ((succ (node-successors node)))
	(if (null succ)
	    (list node)
	    (apply #'append (mapcar #'leaf-nodes succ))))
      (list node)))

(defun expandable-leaf-nodes (node)
  "All leaf nodes reachable from NODE that can be expanded."
  (remove-if #'node-expanded? (leaf-nodes node)))

(defun make-initial-queue (initial-state 
			   &key (queueing-function #'enqueue-at-end))
  (let ((q (make-empty-queue)))
    (funcall queueing-function q (list (make-node :state initial-state)))
    q))

(defun general-search (problem queueing-function)
  "Expand nodes according to the specification of PROBLEM until we find
  a solution or run out of nodes to expand.  The QUEUING-FN decides which
  nodes to look at first."
  (let ((nodes (make-initial-queue (problem-initial-state problem)
				   :queueing-function queueing-function)))
    (let (node)
      (loop (if (empty-queue? nodes) (return nil))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return node))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-bounded-search (problem queueing-function depth)
  "Expand nodes according to the specification of PROBLEM until we
  find a solution or run out of nodes to expand or exceed the
  specified DEPTH.  QUEUING-FN decides which nodes to look at
  first."
  (let ((nodes (make-initial-queue (problem-initial-state problem)
				   :queueing-function queueing-function)))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (> (node-depth node) depth) (return (values nil :cut-off)))
	 (if (goal-test problem node) (return (values t node)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-bounded-search-with-nodes (problem queueing-function depth &optional queue)
  "Expand nodes according to the specification of PROBLEM until we
find a solution or run out of nodes to expand or exceed the specified
DEPTH.  QUEUING-FN decides which nodes to look at first.  QUEUE is a
initial queue of node.  (NIL is an acceptable value for QUEUE.) This
function behaves like a breadth-first search in the sense that as soon
as a node is encountered whose depth exceeds DEPTH, it stops.

Returns three values: (SUCCESS SOLUTION REMAINING-NODES)."
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil nodes)))
	 (setf node (remove-front nodes))
	 (if (> (node-depth node) depth) (return (values nil :cut-off nodes)))
	 (if (goal-test problem node) (return (values t node nodes)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-search-with-nodes (problem queueing-function &optional queue)
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return (values node nodes)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-search-for-bottom (problem queueing-function &optional queue)
  "Expand nodes according to the specification of PROBLEM until we
find a node with no successors or we run out of nodes to expand.  The
QUEUING-FN decides which nodes to look at first."
  (let ((nodes (or queue
		   (make-initial-queue (problem-initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return (values node nodes)))
	 (let ((successors (expand node problem)))
	   (if successors
	       (funcall queueing-function nodes successors)
	       (return (values node nodes))))))))

(defun explain-solution (node)
  "Give the sequence of actions that produced NODE.  When NODE is a
solution to a search problem, this function gives a \"printout\" of
how the node was obtained, starting from an initial node."
  (labels ((explain-backwards (n) 
	     (when (node-parent n)
	       (cons (node-action n)
		     (explain-backwards (node-parent n))))))
    (reverse (explain-backwards node))))

(defun breadth-first-search (problem)
  "Search the shallowest nodes in the search tree first."
  (general-search problem #'enqueue-at-end))

(defun bounded-breadth-first-search (problem depth)
  "Search the shallowest nodes in the search tree first, but don't go
deeper than DEPTH."
  (general-bounded-search problem #'enqueue-at-end depth))

(defun bounded-breadth-first-search-with-nodes (problem depth &optional queue)
  "Search the shallowest nodes in the search tree first, but don't go
deeper than DEPTH.  QUEUE is an (possibly empty) initial pool of
nodes.  NIL is a permissible value for QUEUE."
  (general-bounded-search-with-nodes problem #'enqueue-at-end depth queue))

(defun breadth-first-search-for-bottom-with-nodes (problem &optional queue)
  "Search the shallowest nodes in the search tree first."
  (general-search-for-bottom problem #'enqueue-at-end queue))

(defun breadth-first-search-with-nodes (problem &optional queue)
  "Search the shallowest nodes in the search tree first."
  (general-search-with-nodes problem #'enqueue-at-end queue))

(defun depth-first-search (problem)
  "Search the deepest nodes in the search tree first."
  (general-search problem #'enqueue-at-front))

(defun depth-first-search-for-bottom (problem)
  (general-search-for-bottom problem #'enqueue-at-front))

(defun iterative-deepening-search (problem)
  "Do a series of depth-limited searches, increasing depth each time."
  (loop for depth = 0 do
       (let ((solution (depth-limited-dfs-search problem depth)))
	 (unless (eq solution :cut-off) (return solution)))))

(defun depth-limited-dfs-search (problem &optional limit (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree."
  (cond ((goal-test problem node) node)
        ((and (integerp limit)
	      (>= (node-depth node) limit))
	 :cut-off)
        (t (loop for n in (expand node problem) do
		(let ((solution (depth-limited-dfs-search problem limit n)))
		  (when (and solution
			     (not (eq solution :cut-off)))
		    (return solution)))))))

(defun exhaustive-depth-limited-search (problem &optional limit
			              (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree.
Expand until there are no more nodes of depth less than LIMIT that are
unexpanded."
  (let ((to-do (list node)))
    (until (null to-do)
      (let ((current-node (pop to-do)))
	(when (< (node-depth current-node) limit)
	  (unless (node-expanded? current-node)
	    (expand current-node problem))
	  (dolist (successor (node-successors current-node))
	    (push successor to-do)))))))
						       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Avoiding repeated states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun looping-node? (node &optional depth (test #'equal))
  "Did this node's state appear previously in the path?"
  (let ((n (node-parent node)))
    (if depth
	(loop for i from 1 to depth do
	     (when (null n)
	       (return nil))
	     (when (funcall test (node-state node) (node-state n))
	       (return t))
	     (setf n (node-parent n)))
	(loop for i = 1 do
	     (when (null n)
	       (return nil))
	     (when (funcall test (node-state node) (node-state n))
	       (return t))
	     (setf n (node-parent n))))))

(defun return-node? (node &optional (test #'equal))
  "Is this a node that returns to the state it just came from?"
  (looping-node? node 2 test))

(defun eliminate-returns (nodes)
  "Get rid of nodes that return to the state they just came from,
i.e., where the last two actions just undo each other."
  (remove-if #'return-node? nodes))

(defun eliminate-cycles (nodes &optional (test #'equal))
  "Get rid of nodes that end in a state that has appeared before in
the path."
  (remove-if #'(lambda (node)
		 (looping-node? node nil test))
	     nodes))

(defun eliminate-all-duplicates (nodes node-table)
  "Get rid of all nodes that have been seen before in any path."
  (let ((result nil))
   (loop for node in nodes do
	(let ((state (node-state node)))
	  (when (not (gethash state node-table))
	    (push node result))
	  (setf (gethash state node-table) node)))
   result))

(defun no-cycles-depth-first-search (problem &optional (test #'equal))
  "Do depth-first search, but eliminate paths with repeated states."
  (general-search problem
		  #'(lambda (old-q nodes)
		      (enqueue-at-front old-q 
					(eliminate-cycles nodes
							  test)))))

(defun no-cycles-depth-first-search-for-bottom (problem &optional (test #'equal))
  "Do depth-first search, but eliminate paths with repeated states."
  (general-search-for-bottom problem
			     #'(lambda (old-q nodes)
				 (enqueue-at-front old-q 
						   (eliminate-cycles nodes
								     test)))))

(defun no-returns-breadth-first-search (problem)
  "Do breadth-first search, but eliminate immediate returns to a prior
state."
  (general-search problem
		  #'(lambda (old-q nodes)
		      (enqueue-at-end old-q (eliminate-returns nodes)))))
			      
(defun no-duplicates-breadth-first-search (problem)
  "Do breadth-first search, but eliminate all duplicate states."
  (let ((table (make-hash-table :test #'equal)))
    (general-search problem
		    #'(lambda (old-q nodes)
			(enqueue-at-end old-q (eliminate-all-duplicates
					       nodes table))))))

;;; search.lisp ends here