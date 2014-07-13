;;; search.lisp: Problems, nodes, search trees, search strategies

(in-package :dialogues)

(defclass problem ()
  ((initial-state
    :initarg :initial-state
    :accessor initial-state)
   (goal
    :initarg :goal
    :accessor problem-goal)
   (num-expanded
    :accessor problem-num-expanded
    :initform 0))
  (:documentation "A problem is defined by the initial state, and the type of problem it is.  For bookkeeping, we count the number of nodes expanded."))

(defclass node ()
  ((state
    :accessor state
    :initarg :state
    :documentation "A stte in the domain")
   (parent
    :initform nil
    :initarg :parent
    :accessor parent
    :type (or null node)
    :documentation "The parent node of this node")
   (action
    :accessor action
    :initarg :action
    :documentation "The action leading to this state.")
   (successors
    :type list
    :initarg :successors
    :accessor successors
    :documentation "A list of successor nodes.")
   (depth
    :initform 0
    :initarg :depth
    :accessor depth
    :type integer
    :documentation "Depth of the node in the tree (root = 0).")
   (expanded-p
    :initform nil
    :type boolean
    :accessor expanded-p
    :initarg :expanded-p
    :documentation "Has this node been expanded?"))
  (:documentation "Node for generic search.  A node contains a state, a domain-specific representation of a point in the search space.  It also contains some bookkeeping information."))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A" (state node))))

(defun node-p (x)
  "Is X a NODE?"
  (typep x 'dialogues::node))

(defgeneric successors-in-problem (problem node)
  (:documentation "Return an alist of (action . state) pairs, reachable from this state."))

(defmethod successors-in-problem ((problem problem) node)
  (declare (ignore node))
  (error "You need to define a SUCCESSORS-IN-PROBLEM method for the problem~%~%  ~a~%" problem))

(defmethod goal-test ((problem problem) node)
  "Return true or false: is this a goal node?  This default method checks if the state is equal to the state stored in the problem-goal slot.  You will need to define your own method if there are multiple goals, or if you need to compare them with something other than EQUAL."
  (equal (state node) (problem-goal problem)))

(defun node-ancestors (node)
  "The ancestors of NODE, starting with its most distant ancestor (i.e., the ancestor of NODE whose parent is NIL)."
  (labels ((node-ancestors-backwards (n)
	     (if (parent n)
		 (cons n (node-ancestors-backwards (parent n)))
		 (list n))))
    (reverse (node-ancestors-backwards (parent node)))))

(defun make-successor-node (parent action state)
  "Make a successor of PARENT that is arrived at by taking ACTION and yielding STATE."
  (make-instance 'dialogues::node
                 :parent parent
                 :action action
                 :state state
                 :depth (1+ (depth parent))))

(defun expand (node problem)
  (loop
     :initially (when (expanded-p node) (return (successors node)))
     :for (action . state) :in (successors-in-problem problem node)
     :collect (make-successor-node node action state) :into nodes
     :finally
     (setf (successors node) nodes
           (expanded-p node) t)
     (incf (problem-num-expanded problem))
     (return nodes)))

(defun create-start-node (problem)
  "Make the starting node, corresponding to the problem's initial state."
  (make-instance 'dialogues::node
                 :state (initial-state problem)))

(defun leaf-nodes (node)
  "All nodes reachable from NODE (via the successor function) that are either unexpanded or have no successors (and are expanded)."
  (if (expanded-p node)
      (let ((succ (successors node)))
	(if (null succ)
	    (list node)
	    (apply #'append (mapcar #'leaf-nodes succ))))
      (list node)))

(defun expandable-leaf-nodes (node)
  "All leaf nodes reachable from NODE that can be expanded."
  (remove-if #'expanded-p (leaf-nodes node)))

(defun first-splitting-descendant (node)
  "The first descendant of NODE that has multiple successors.  If there are no such nodes (i.e., the set of descendents of NODE forms a linear sequence), return NIL."
  (let ((succs (successors node)))
    (if (null succs)
	nil
	(if (null (cdr succs))
	    (first-splitting-descendant (first succs))
	    node))))

(defun make-initial-queue (initial-state
			   &key (queueing-function #'enqueue-at-end))
  (let ((q (make-empty-queue)))
    (funcall queueing-function q (list (make-instance 'dialogues::node
                                                      :state initial-state)))
    q))

(defun general-search (problem queueing-function)
  "Expand nodes according to the specification of PROBLEM until we find a solution or run out of nodes to expand.  The QUEUING-FN decides which nodes to look at first."
  (let ((nodes (make-initial-queue (initial-state problem)
				   :queueing-function queueing-function)))
    (let (node)
      (loop (if (empty-queue? nodes) (return nil))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return node))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-bounded-search (problem queueing-function depth)
  "Expand nodes according to the specification of PROBLEM until we find a solution or run out of nodes to expand or exceed the specified DEPTH.  QUEUING-FN decides which nodes to look at first."
  (let ((nodes (make-initial-queue (initial-state problem)
				   :queueing-function queueing-function)))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (> (depth node) depth) (return (values nil :cut-off)))
	 (if (goal-test problem node) (return (values t node)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-bounded-search-with-nodes (problem queueing-function depth &optional queue)
  "Expand nodes according to the specification of PROBLEM until we find a solution or run out of nodes to expand or exceed the specified DEPTH.  QUEUING-FN decides which nodes to look at first.  QUEUE is a initial queue of node.  (NIL is an acceptable value for QUEUE.) This function behaves like a breadth-first search in the sense that as soon as a node is encountered whose depth exceeds DEPTH, it stops.

Returns three values: (SUCCESS SOLUTION REMAINING-NODES)."
  (let ((nodes (or queue
		   (make-initial-queue (initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil nodes)))
	 (setf node (remove-front nodes))
	 (if (> (depth node) depth) (return (values nil :cut-off nodes)))
	 (if (goal-test problem node) (return (values t node nodes)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-search-with-nodes (problem queueing-function &optional queue)
  (let ((nodes (or queue
		   (make-initial-queue (initial-state problem)
				       :queueing-function queueing-function))))
    (let (node)
      (loop (if (empty-queue? nodes) (return (values nil nil)))
	 (setf node (remove-front nodes))
	 (if (goal-test problem node) (return (values node nodes)))
	 (funcall queueing-function nodes (expand node problem))))))

(defun general-search-for-bottom (problem queueing-function &optional queue)
  "Expand nodes according to the specification of PROBLEM until we find a node with no successors or we run out of nodes to expand.  The QUEUING-FN decides which nodes to look at first."
  (let ((nodes (or queue
		   (make-initial-queue (initial-state problem)
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
  "Give the sequence of actions that produced NODE.  When NODE is a solution to a search problem, this function gives a \"printout\" of how the node was obtained, starting from an initial node."
  (labels ((explain-backwards (n)
	     (when (parent n)
	       (cons (action n)
		     (explain-backwards (parent n))))))
    (reverse (explain-backwards node))))

(defun breadth-first-search (problem)
  "Search the shallowest nodes in the search tree first."
  (general-search problem #'enqueue-at-end))

(defun bounded-breadth-first-search (problem depth)
  "Search the shallowest nodes in the search tree first, but don't go deeper than DEPTH."
  (general-bounded-search problem #'enqueue-at-end depth))

(defun bounded-breadth-first-search-with-nodes (problem depth &optional queue)
  "Search the shallowest nodes in the search tree first, but don't go deeper than DEPTH.  QUEUE is an (possibly empty) initial pool of nodes.  NIL is a permissible value for QUEUE."
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
  (loop
     :for depth :from 0
     :for solution = (depth-limited-dfs-search problem depth)
     :unless (eq solution :cut-off) :do (return solution)))

(defun depth-limited-dfs-search (problem &optional limit (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree."
  (cond ((goal-test problem node) node)
        ((and (integerp limit)
	      (>= (depth node) limit))
	 :cut-off)
        (t (loop
              :for n :in (expand node problem)
              :for solution = (depth-limited-dfs-search problem limit n)
              :when (and solution (not (eq solution :cut-off)))
              :do (return solution)))))

(defun exhaustive-depth-limited-search (problem &optional limit
			              (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree. Expand until there are no more nodes of depth less than LIMIT that are unexpanded."
  (let ((to-do (list node)))
    (until (null to-do)
      (let ((current-node (pop to-do)))
	(when (< (depth current-node) limit)
	  (unless (expanded-p current-node)
	    (expand current-node problem))
	  (dolist (successor (successors current-node))
	    (push successor to-do)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Avoiding repeated states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun looping-node? (node &optional depth (test #'equal))
  "Did this node's state appear previously in the path?"
  (loop
     :with n = (parent node)
     :with i = 1
     :do
     (cond ((and (integerp depth) (> i depth))
            (return nil))
           ((null n)
            (return nil))
           ((funcall test (state node) (state n))
            (return t))
           (t
            (setf n (parent n))
            (incf i)))))

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
  (flet ((loopy (node)
            (looping-node? node nil test)))
    (remove-if #'loopy nodes)))

(defun eliminate-all-duplicates (nodes node-table)
  "Get rid of all nodes that have been seen before in any path."
  (loop
     :with result = nil
     :for node :in nodes
     :for state = (state node)
     :do
     (when (not (gethash state node-table))
       (push node result))
     (setf (gethash state node-table) node)
     :finally (return result)))

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
