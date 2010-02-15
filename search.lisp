;;; search.lisp: Nodes, search trees, informed and uninformed search strategies
;;
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
  (g-cost 0)                ; path cost from root to node
  (h-cost 0)                ; estimated distance from state to goal
  (f-cost 0)                ; g-cost + h-cost
  )

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

(defun expand (node &key successor-function
	                 (edge-cost-function #'always-one)
	                 (heuristic-function #'always-zero))
  "Generate a list of all the nodes that can be reached from NODE.
SUCCESSOR-FUNCTION should be a unary function on nodes that evaluates
to a list of (action . state) pairs.  Return a list of nodes for
further consideration.  If NODE has already been expanded, return the
empty list.  One can optionally specify an edge-cost function and a
heuristic function."
  (unless (node-expanded? node)
    (setf (node-expanded? node) t)
    (let ((nodes nil)
	  (successors (funcall successor-function node)))
      (setf (node-successors node) successors)
      (dolist (successor successors nodes)
	(let ((successor-action (car successor))
	      (successor-state (cdr successor)))
	  (let ((g (+ (node-g-cost node) 
		      (funcall edge-cost-function node 
			                          successor-action
						  successor-state)))
		(h (funcall heuristic-function successor-state)))
	    (let ((new-node
		   (make-node :state successor-state
			      :parent node
			      :action successor-action
			      :depth (1+ (node-depth node))
			      :g-cost g
			      :h-cost h
			      :f-cost (max (node-f-cost node) (+ g h)))))
	    (push new-node nodes))))))))

(defun make-initial-queue (initial-state &key queueing-function)
  (let ((q (make-empty-queue)))
    (funcall queueing-function q (list (make-node :state initial-state)))
    q))

(defun general-search (initial-state &key queueing-function 
		                          goal-test 
		                          successor-function
		                          (edge-cost-function #'always-one)
		                          (heuristic-function #'always-zero))
  "Expand nodes using SUCCESSOR-FUNCTION until we find a solution that
satisfies GOAL-TEST or run out of nodes to expand.  QUEUING-FUNCTION
decides which nodes to look at first. EDGE-COST-FUNCTION is a function
of three arguments: a node, an action, and a state; it is indended to
represent the cost of going from the node to the next state by taking
the action.  By default, it is the constant function 1.
HEURISTIC-FUNCTION, is a unary function that takes a state and
estimates the number of steps it is from a goal state.  By default, it
is the constant function 0."
  (let ((nodes-to-process (make-initial-queue
			   initial-state
			   :queueing-function queueing-function))
	(node nil))
    (loop (if (empty-queue? nodes-to-process) (return nil))
	  (setf node (remove-front nodes-to-process))
	  (if (funcall goal-test node) (return node))
          (funcall queueing-function 
		   nodes-to-process
		   (expand 
		    node 
		    :successor-function successor-function
		    :edge-cost-function edge-cost-function
		    :heuristic-function heuristic-function)))))
				      

(defun explain-solution (node)
  "Give the sequence of actions that produced NODE.  When NODE is a
solution to a search problem, this function gives a \"printout\" of
how the node was obtained, starting from an initial node."
  (labels ((explain-backwards (n) 
	     (when (node-parent n)
	       (cons (node-action n)
		     (explain-backwards (node-parent n))))))
    (reverse (explain-backwards node))))
						       
(defun depth-first-search (initial-state &key goal-test successor-function)
  (general-search initial-state :goal-test goal-test
		                :successor-function successor-function
				:queueing-function #'enqueue-at-front))

(defun breadth-first-search (initial-state &key goal-test successor-function)
  (general-search initial-state :goal-test goal-test
		                :successor-function successor-function
				:queueing-function #'enqueue-at-end))

(defun depth-limited-search (initial-state limit &key goal-test 
			                              successor-function)
  (depth-limited-search-helper (make-node :state initial-state)
			       limit
			       :goal-test goal-test
			       :successor-function successor-function))

(defun depth-limited-search-helper (node limit &key goal-test
				                    successor-function)
  (cond ((funcall goal-test node) node)
        ((>= (node-depth node) limit) nil)
        (t (loop 
	      for n in (expand node :successor-function successor-function) do
		(let ((solution (depth-limited-search-helper 
				 n
				 limit
				 :goal-test goal-test
				 :successor-function successor-function)))
		  (when solution
		    (return solution)))))))

(defun iterative-deepening-search (initial-state &key goal-test
				                      successor-function)
  (loop for depth from 1 do
       (let ((solution (depth-limited-search initial-state depth
		          :goal-test goal-test
			  :successor-function successor-function)))
	 (unless (eq solution 'cutoff)
	   (return solution)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Avoiding repeated states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun looping-node? (node &optional depth)
  "Did this node's state appear previously in the path?"
  (let ((n (node-parent node)))
    (if depth
	(loop for i from 1 to depth do
	     (when (null n) (return nil))
	     (when (equal (node-state node) (node-state n)) (return t))
	     (setf n (node-parent n)))
	(loop for i = 1 do
	     (when (null n) (return nil))
	     (when (equal (node-state node) (node-state n)) (return t))
	     (setf n (node-parent n))))))

(defun return-node? (node)
  "Is this a node that returns to the state it just came from?"
  (looping-node? node 2))

(defun eliminate-returns (nodes)
  "Get rid of nodes that return to the state they just came from,
i.e., where the last two actions just undo each other."
  (remove-if #'return-node? nodes))

(defun eliminate-cycles (nodes)
  "Get rid of nodes that end in a state that has appeared before in
the path."
  (remove-if #'looping-node? nodes))

(defun eliminate-all-duplicates (nodes node-table)
  "Get rid of all nodes that have been seen before in any path."
  (let ((result nil))
   (loop for node in nodes do
	(let ((state (node-state node)))
	  (when (not (gethash state node-table))
	    (push node result))
	  (setf (gethash state node-table) node)))
   result))

(defun no-cycles-depth-first-search (initial-state &key successor-function
				                        goal-test)
  "Do depth-first search, but eliminate paths with repeated states."
  (general-search initial-state
		  :successor-function successor-function
		  :queueing-function 
  		    #'(lambda (old-q nodes)
			(enqueue-at-front old-q 
					  (eliminate-cycles nodes)))
		  :goal-test goal-test))

(defun no-returns-breadth-first-search (initial-state &key successor-function
					                   goal-test)
  "Do breadth-first search, but eliminate immediate returns to a prior
state."
  (general-search initial-state
		  :successor-function successor-function
		  :queueing-function
  		    #'(lambda (old-q nodes)
			(enqueue-at-end old-q (eliminate-returns nodes)))
		  :goal-test goal-test))
			      
(defun no-duplicates-breadth-first-search (initial-state 
					   &key successor-function
					        goal-test)
  "Do breadth-first search, but eliminate all duplicate states."
  (let ((table (make-hash-table :test #'equal)))
    (general-search initial-state
		    :successor-function successor-function
		    :queueing-function
		      #'(lambda (old-q nodes)
			  (enqueue-at-end old-q (eliminate-all-duplicates
						 nodes table)))
		    :goal-test goal-test)))

;;; Heuristic search

(defun best-first-search (initial-state evaluation-function
			  &key successor-function
			       goal-test
			       heuristic-function
			       (edge-cost-function #'(lambda (n a s)
						       (declare (ignore n a s))
						       1)))
  "Search the nodes with the best evaluation first. [p 93]"
  (general-search initial-state
		  :successor-function successor-function
		  :goal-test goal-test
		  :queueing-function 
		    #'(lambda (old-q nodes) 
			(enqueue-by-priority old-q nodes evaluation-function))
		  :edge-cost-function edge-cost-function
		  :heuristic-function heuristic-function))

(defun greedy-search (initial-state heuristic-function &key successor-function 
		                                            goal-test)
  "Best-first search using H (heuristic distance to goal). [p 93]"
  (best-first-search initial-state #'node-h-cost
		     :successor-function successor-function
		     :goal-test goal-test
		     :heuristic-function heuristic-function))

(defun tree-a*-search (initial-state heuristic-function
		       &key successor-function goal-test)
  "Best-first search using estimated total cost, or (F = G + H). [p 97]"
  (best-first-search initial-state #'node-f-cost
		     :successor-function successor-function
		     :goal-test goal-test
		     :heuristic-function heuristic-function))

(defun uniform-cost-search (initial-state 
			    &key successor-function 
			         goal-test)
  "Best-first search using the node's depth as its cost.  Discussion on [p 75]"
  (best-first-search initial-state #'node-depth
		     :successor-function successor-function
		     :goal-test goal-test))

(defun make-eliminating-queuing-fn (evaluation-function)
  (let ((table (make-hash-table :test #'equal)))
    #'(lambda (old-q nodes)
	(enqueue-by-priority
	 old-q
	 (let ((result nil))
	   (loop for node in nodes do
		(let ((old-node (gethash (node-state node) table)))
		  (cond
		    ((null old-node)
		     ;; First time we've reached state; just return node
		     (setf (gethash (node-state node) table) node)
		     (push node result))
		    ((<= (funcall evaluation-function old-node)
			 (funcall evaluation-function node))
		     ;; If the old node is better, discard the new node
		     nil)
		    (t ;; Otherwise, discard the old node
		     (setf (node-expanded? old-node) t)
		     (setf (gethash (node-state node) table) node)
		     (push node result)))))
	   (nreverse result))
	 evaluation-function))))

(defun A*-search (initial-state heuristic-function
		  &key goal-test 
		       successor-function
		       (edge-cost-function #'(lambda (n a s)
					       (declare (ignore n a s))
					       1)))
  "Starting from INITIAL-STATE, search the nodes with the best f cost
  first.  If a node is ever reached by two different paths, keep only
  the better path."
  (general-search 
   initial-state
   :goal-test goal-test
   :successor-function successor-function
   :queueing-function (make-eliminating-queuing-fn #'node-f-cost)
   :heuristic-function heuristic-function
   :edge-cost-function edge-cost-function))
     
;;; search.lisp ends here