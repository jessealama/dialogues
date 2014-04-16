;;; dialogue-search.lisp Dialogue games as search trees

(in-package :dialogues)

(defstruct (dialogue-search-problem
	     (:include problem))
  (rules nil :type (or (eql nil) ruleset)))

(defmethod goal-test ((problem dialogue-search-problem) node)
  (expand node problem)
  (and (null (node-successors node))
       (proponent-move? (last-move (node-state node)))))

(defmethod successors ((dsp dialogue-search-problem) node)
  (let ((dialogue-so-far (node-state node))
	(extensions nil))
    (dolist (player '(p o) extensions)
      (dolist (stance '(a d))
	(let ((next-moves (next-moves dialogue-so-far player stance)))
	  (push-all (mapcar #'(lambda (next-move)
				(destructuring-bind (statement index)
				    next-move
				  (cons (list player stance index statement)
					(freshly-extend-dialogue dialogue-so-far
								 player
								 stance
								 statement
								 index))))
			    next-moves)
		    extensions))))))

(defun dialogue-search-bfs (rules initial-statement &optional more-nodes)
  (let* ((initial-state (make-dialogue initial-statement rules))
         (problem (make-dialogue-search-problem :initial-state initial-state
                                                :rules rules)))
    (breadth-first-search-for-bottom-with-nodes problem more-nodes)))

(defun dialogue-search-dfs (rules initial-statement)
  (let* ((initial-state (make-dialogue initial-statement rules))
         (problem (make-dialogue-search-problem :initial-state initial-state
                                                :rules rules)))
    (depth-first-search-for-bottom problem)))

(defun dialogue-search-dfs-no-cycles (rules initial-statement)
  (let* ((initial-state (make-dialogue initial-statement rules))
         (problem (make-dialogue-search-problem :initial-state initial-state
                                                :rules rules)))
    (no-cycles-depth-first-search-for-bottom problem #'equal-dialogues?)))

(defun bounded-dialogue-search-dfs (rules initial-statement depth &optional (initial-state (make-dialogue initial-statement rules)))
  (let ((problem (make-dialogue-search-problem :initial-state initial-state
                                               :rules rules)))
    (depth-limited-dfs-search problem depth)))

(defun bounded-dialogue-search-bfs (rules initial-statement depth
				    &optional (initial-state (make-dialogue initial-statement rules))
				              initial-queue)
(let ((problem (make-dialogue-search-problem :initial-state initial-state
						   :rules rules)))
	(bounded-breadth-first-search-with-nodes problem depth initial-queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (dialogue-strategy-search-problem
	     (:include problem))
  (rules nil :type list))

(defun strategy-successors (strategy rules)
  (declare (ignore strategy rules))
  nil)

(defun some-non-cutoff-result (pred list)
  (if (null list)
      nil
      (loop
	 with all-are-cutoffs = t
	 for search-result in list
	 do
	   (let ((result (funcall pred search-result)))
             (setf all-are-cutoffs (and all-are-cutoffs
                                        (eq result :cutoff)))
	     (unless (eq result :cutoff)
	       (unless (null result)
		 (return t))))
	 finally
	   (if all-are-cutoffs
	       (return :cutoff)
	       (return nil)))))

(defun every-disallowing-cutoffs (pred list)
  (loop
     for elt in list
     do
       (let ((result (funcall pred elt)))
	 (when (eq result :cutoff)
	   (return :cutoff))
	 (when (null result)
	   (return nil)))
     finally
       (return t)))

(defun proponent-has-winning-strategy? (dialogue cutoff &optional (start 1))
  (cond ((minusp cutoff) :cutoff)
	((zerop cutoff)
	 (if (evenp start)
	     :cutoff
	     (or (null (all-next-opponent-moves-at-position dialogue start))
		 :cutoff)))
	(t (let ((every-result
		  (every-disallowing-cutoffs
		   #'(lambda (opponent-move)
		       (let ((dialogue-opponent
			      (add-move-to-dialogue-at-position dialogue
								opponent-move
								start)))
			 (some-non-cutoff-result
			  #'(lambda (proponent-move)
			      (let ((dialogue-proponent
				     (add-move-to-dialogue-at-position dialogue
								       proponent-move
								       (1+ start))))
				(proponent-has-winning-strategy? dialogue-proponent
								 (- cutoff 2)
								 (+ start 2))))
			  (all-next-proponent-moves-at-position dialogue-opponent
								(+ start 1)))))
		   (all-next-opponent-moves-at-position dialogue start))))
	     (if (eq every-result :cutoff)
		 :cutoff
		 every-result)))))

(defun proponent-has-winning-strategy--defenses-preferred? (dialogue cutoff &optional (start 1))
  (cond ((minusp cutoff) :cutoff)
	((zerop cutoff)
	 (if (evenp start)
	     :cutoff
	     (or (null (all-next-opponent-moves-at-position dialogue start))
		 :cutoff)))
	(t (let ((every-result
		  (every-disallowing-cutoffs
		   #'(lambda (opponent-move)
		       (let ((dialogue-opponent
			      (add-move-to-dialogue-at-position dialogue
								opponent-move
								start)))
			 (some-non-cutoff-result
			  #'(lambda (proponent-move)
			      (let ((dialogue-proponent
				     (add-move-to-dialogue-at-position dialogue
								       proponent-move
								       (1+ start))))
				(proponent-has-winning-strategy--defenses-preferred? dialogue-proponent
                                                                                     (- cutoff 2)
                                                                                     (+ start 2))))
			  (let ((all-pro-moves (all-next-proponent-moves-at-position dialogue-opponent
                                                                                     (+ start 1))))
                            (let ((defense (find-if #'defensive-move? all-pro-moves)))
                              (if defense
                                  (progn
                                    ;; (break "a defense is available in~%~a~%picking it" all-pro-moves)
                                    (list defense))
                                  all-pro-moves))))))
		   (all-next-opponent-moves-at-position dialogue start))))
	     (if (eq every-result :cutoff)
		 :cutoff
		 every-result)))))

(defun dialogue-valid? (rules statement depth)
  "Determine whether statement is valid in the dialogue games defined by
RULES.  \"Valid\" means that Proponent has a winning strategy in the
game tree developed down to depth DEPTH."
  (let ((dialogue (make-dialogue statement rules)))
    (if (fast-eval-entire-dialogue dialogue)
	(proponent-has-winning-strategy? dialogue depth 1)
	(error 'inappropriate-initial-statement-error
	       :rules rules
	       :statement statement))))

(defun classify-formulas (formulas rules search-depth)
  (let (inappropriate valid invalid cutoff)
    (dolist (formula formulas)
      (handler-case (let ((result (dialogue-valid? rules
						   formula
						   search-depth)))
		      (ecase result
			(:cutoff (push formula cutoff))
			(t (push formula valid))
			((nil) (push formula invalid))))
	(inappropriate-initial-statement-error ()
	  (push formula inappropriate))))
    (values inappropriate valid invalid cutoff)))

;; Functionality dropped.  But it might be nice to restore it later.
;;
;; (defun classify-all-formulas (rules signature formula-depth search-depth)
;;   (classify-formulas (reduced-enumeration formula-depth signature)
;; 		     rules
;; 		     signature
;; 		     search-depth))

;;; Strategy search for specific rulesets

(defun intuitionistically-valid--e? (formula depth)
  (let ((dialogue (make-dialogue formula e-dialogue-rules)))
    (if (and (implication? formula)
             (binary-conjunction? (antecedent formula)))
        (let ((a (antecedent formula))
              (c (consequent formula)))
          (let ((a-1 (lhs a))
                (a-2 (rhs a)))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a 'a 0))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move *attack-left-conjunct* 'a 1))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a-1 'd 2))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move *attack-right-conjunct* 'a 1))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a-2 'd 4))
            (when (atomic-formula? c)
              (error "Don't know how to deal with the atomic consequent of the given formula~%~%  ~a~%" formula))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move c 'd 1))
            ;(break "dialogue looks like:~%~A" dialogue)
            (proponent-has-winning-strategy? dialogue depth 7)))
        (proponent-has-winning-strategy? dialogue depth 1))))

(defun intuitionistically-valid--e-no-pro-repeats? (formula depth)
(let ((dialogue (make-dialogue formula e-dialogue-rules-no-pro-repetitions)))
    (if (and (implication? formula)
             (binary-conjunction? (antecedent formula)))
        (let ((a (antecedent formula))
              (c (consequent formula)))
          (let ((a-1 (lhs a))
                (a-2 (rhs a)))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a 'a 0))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move *attack-left-conjunct* 'a 1))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a-1 'd 2))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move *attack-right-conjunct* 'a 1))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a-2 'd 4))
            (when (atomic-formula? c)
              (error "Don't know how to deal with the atomic consequent of the given formula~%~%  ~a~%" formula))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move c 'd 1))
            ;(break "dialogue looks like:~%~A" dialogue)
            (proponent-has-winning-strategy? dialogue depth 7)))
        (proponent-has-winning-strategy? dialogue depth 1))))

(defun intuitionistically-valid--e-no-repeats? (formula depth)
  (let ((dialogue (make-dialogue formula e-dialogue-rules-no-repetitions)))
    (if (and (implication? formula)
             (binary-conjunction? (antecedent formula)))
        (let ((a (antecedent formula))
              (c (consequent formula)))
          (let ((a-1 (lhs a))
                (a-2 (rhs a)))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a 'a 0))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move *attack-left-conjunct* 'a 1))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a-1 'd 2))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move *attack-right-conjunct* 'a 1))
            (add-move-to-dialogue dialogue
                                  (make-opponent-move a-2 'd 4))
            (when (atomic-formula? c)
              (error "Don't know how to deal with the atomic consequent of the given formula~%~%  ~a~%" formula))
            (add-move-to-dialogue dialogue
                                  (make-proponent-move c 'd 1))
            ;(break "dialogue looks like:~%~A" dialogue)
            (proponent-has-winning-strategy? dialogue depth 7)))
        (proponent-has-winning-strategy? dialogue depth 1))))

(defun intuitionistically-valid--e-no-pro-double-repeats? (formula depth)
  (dialogue-valid? e-dialogue-rules-no-pro-double-repetitions
                   formula
                   depth))

(defun intuitionistically-valid--d? (formula depth)
  (dialogue-valid? d-dialogue-rules
                   formula
                   depth))

;; (defun defenses--e (strategy-node)
;;   (unless (attack-p strategy-node)
;;     (error "The move of a strategy node is not an attack, so how can we defend against it?"))
;;   (let ((dialogue (strategy-node->dialogue strategy-node)))
;;     (let (()))))

(defun defend--e (strategy-node)
  (first (defenses--e strategy-node)))

(defun e-prefer-defenses (node expander max-depth)
  "Expand a Proponent node according to EXPANDER.  The maximum depth of terms (variables, constants, function terms) will be MAX-DEPTH.

Defenses are preferred in the sense that if a defensive move can be made, return it as the sole available move.  (If multiple defenses are available, pick one arbitrarily.)"
  ;; is the last move by Opp an attack against which we can defend?
  (let ((p (parent node)))
    (when (attack-p p)
      (list (defend p)))))

;; (defun intuitionistically-valid? (formula strategy-depth)
;;   (when (atomic-formula-p formula)
;;     (return nil))
;;   (loop
;;      :for term-depth :from 0
;;      :when (intuitionistically-valid-wrt-strategy formula #'(lambda (node) (e-prefer-defenses node term-depth)) strategy-depth) :do (return t)))

;; constrained search (not just constrained rulesets)

(defun intuitionistically-valid--e-no-repeats--prefer-defenses? (formula depth)
  (let ((dialogue (make-dialogue formula e-dialogue-rules-no-repetitions)))
    (proponent-has-winning-strategy--defenses-preferred? dialogue depth 1)))

(defvar search-tree-directory (make-hash-table :test #'equal)
  "A mapping from triples (FORMULA RULESET DEPTH) to search trees.

It is represented as an association list with entries of the
form (FORMULA . STUFF), where STUFF is a list of triples (RULESET
DEPTH TREE).")

(defun search-trees-for-formula (formula)
  (gethash formula search-tree-directory nil))

(defun search-trees-for-formula-with-rules (formula rules)
  (remove-if-not #'(lambda (entry)
		     (destructuring-bind (ruleset depth tree)
			 entry
		       (declare (ignore depth tree))
		       (equal-rulesets? ruleset rules)))
		 (search-trees-for-formula formula)))

(defun search-tree-for-formula-with-depth (formula depth)
  (remove-if-not #'(lambda (entry)
		     (destructuring-bind (ruleset d tree)
			 entry
		       (declare (ignore ruleset tree))
		       (= depth d)))
		 (search-trees-for-formula formula)))

(defun register-dialogue-tree (formula rules depth tree)
  (let ((earlier (gethash formula search-tree-directory))
	(entry (list rules depth tree)))
    (cond ((null earlier) ; we've never seen FORMULA
	   (setf (gethash formula search-tree-directory) (list entry)))
	  (t
	   (setf (gethash formula search-tree-directory)
		 (append (list entry) earlier))))))

(defun develop-dialogue-tree-to-depth (tree-root depth problem)
  (let ((expandable-leaves (expandable-leaf-nodes tree-root)))
    (dolist (leaf expandable-leaves tree-root)
      (exhaustive-depth-limited-search problem depth leaf))))

(defun dialogue-search-tree (formula rules depth)
  (let ((earlier-entries (search-trees-for-formula-with-rules formula rules))
	(problem (make-dialogue-search-problem :rules rules
                                               :initial-state (make-dialogue formula

                                                                             rules))))
    (let ((tree (loop
		   for entry in earlier-entries
		   do
		     (destructuring-bind (ruleset d root)
			 entry
		       (when (equal-rulesets? ruleset rules)
			 (when (>= d depth)
			   (return root))))
		   finally
		     (return (create-start-node problem)))))
      (cond ((>= (node-depth tree) depth)
	     tree)
	    (t
	     (let ((new-tree (develop-dialogue-tree-to-depth tree depth problem)))
	       (register-dialogue-tree formula rules depth new-tree)
	       new-tree))))))

(defun copy-search-tree-node (node)
  (make-node :state (node-state node)
	     :parent (node-parent node)
	     :action (node-action node)
	     :successors (mapcar #'copy-search-tree-node (node-successors node))
	     :depth (node-depth node)
	     :expanded? (node-expanded? node)))

(defun dialogue->search-tree (dialogue)
  "Construct a search tree (a sequence, in fact) from DIALOGUE."
  (let (nodes)
    (dotimes (i (dialogue-length dialogue)) ; first construct the nodes
      (push (make-node :state (truncate-dialogue dialogue i)
		       :parent nil
		       :action nil
		       :successors nil
		       :depth i)
	    nodes))
    (setf nodes (nreverse nodes))
    (loop
       for node-parent in nodes
       for node-successor in (cdr nodes)
       do
	 (setf (node-parent node-successor) node-parent
	       (node-successors node-parent) (list node-successor)
	       (node-expanded? node-parent) t))
    (car (last nodes))))

(defmethod proponent-node? ((node node))
  (proponent-move? (last-move (node-state node))))

(defmethod opponent-node? ((node node))
  (opponent-move? (last-move (node-state node))))

(defun proponent-ws-from-opponent-node (opponent-node &optional ruleset)
  "Find a winning strategy from OPPONENT-NODE, which is supposed to
represent a move just played by opponent.
Return :DIALOGUE-TREE-TOO-SHALLOW if there are any unexpanded nodes that,
if expanded, could make a difference in the determination of the
existence of a winning strategy.  Return NIL if there are no winning
strategies for Proponent starting from OPPONENT-NODE.  If there are,
return a copy of OPPONENT-NODE that contains the winning strategy.  It
is assumed that OPPONENT-NODE is expanded."
  (if (and (eq ruleset nearly-classical-dialogue-rules)
	   (let* ((state (node-state opponent-node))
		  (last-move (last-move state)))
	     (defensive-move? last-move)))
      nil
      (let ((opponent-succs (node-successors opponent-node)))
	(if (node-expanded? opponent-node)
	    (if (null opponent-succs)
		nil
		(let ((strategies (mapcar #'(lambda (node)
					      (proponent-ws-from-proponent-node node ruleset))
					  opponent-succs)))
		  (if (every #'null strategies)
		      nil
		      (let ((maybe-winner (find-if #'node-p strategies)))
			(if (node-p maybe-winner)
			    (make-node :state (node-state opponent-node)
				       :parent (node-parent opponent-node)
				       :action (node-action opponent-node)
				       :successors (list maybe-winner)
				       :depth (node-depth opponent-node)
				       :expanded? t)
			    :dialogue-tree-too-shallow)))))
	    :dialogue-tree-too-shallow))))

(defun proponent-ws-from-proponent-node (proponent-node &optional ruleset)
  (if (node-expanded? proponent-node)
      (let ((succs (node-successors proponent-node)))
	(let ((strategies (mapcar #'(lambda (node)
				      (proponent-ws-from-opponent-node node ruleset))
				  succs)))
	  (if (member nil strategies)
	      nil
	      (if (member :dialogue-tree-too-shallow strategies)
		  :dialogue-tree-too-shallow
		  (make-node :state (node-state proponent-node)
			     :parent (node-parent proponent-node)
			     :action (node-action proponent-node)
			     :successors strategies
			     :depth (node-depth proponent-node)
			     :expanded? t)))))
      :dialogue-tree-too-shallow))

(defvar winning-strategy-registry (make-hash-table :test #'equalp))

(defun winning-strategy (formula ruleset depth &optional dialogue)
  (let* ((problem (make-dialogue-search-problem :rules ruleset
						:initial-state (or dialogue
								   (make-dialogue formula
										  ruleset))))
	 (root (develop-dialogue-tree-to-depth (create-start-node problem) depth problem)))
    (proponent-ws-from-proponent-node root ruleset)))

(defun explain-strategy (winning-strategy)
  (declare (ignore winning-strategy))
  (format t "implement something sensible here"))


;;; dialogue-search.lisp ends here
