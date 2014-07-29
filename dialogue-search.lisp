;;; dialogue-search.lisp Dialogue games as search trees

(in-package :dialogues)

(defclass dialogue-search-problem (problem)
  ((formula
    :type formula
    :accessor formula
    :initform (error "A dialogue search problem requires an initial formula.")
    :initarg :formula)
   (ruleset
    :type ruleset
    :accessor ruleset
    :initform (error "A dialogue search problem requires a ruleset.")
    :initarg :ruleset)
   (concessions
    :type list
    :initform nil
    :initarg :concessions
    :reader concessions
    :documentation "A list of formulas granted by the Opponent prior to the start of the game.")))

(defmethod initialize-instance :after ((dsp dialogue-search-problem) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (initial-node dsp)
        (make-instance 'dialogue-node :state (formula dsp))))

(defmethod successors-in-problem ((dsp dialogue-search-problem)
                                  (node dialogue-node))
  (let ((ruleset (ruleset dsp)))
    (funcall (expander ruleset) node)))

(defun dialogue-search-bfs (rules initial-statement &optional more-nodes)
  (let* ((problem (make-instance 'dialogue-search-problem
                                 :formula initial-statement
                                 :ruleset rules)))
    (breadth-first-search-for-bottom-with-nodes problem more-nodes)))

(defun dialogue-search-dfs (rules initial-statement)
  (let* ((problem (make-instance 'dialogue-search-problem
                                 :formula initial-statement
                                 :ruleset rules)))
    (depth-first-search-for-bottom problem)))

(defun bounded-dialogue-search-dfs (rules initial-statement depth)
  (let ((problem (make-instance 'dialogue-search-problem
                                :formula initial-statement
                                :ruleset rules)))
    (depth-limited-dfs-search problem depth)))

(defun bounded-dialogue-search-bfs (rules initial-statement depth)
  (let* ((node (make-instance 'dialogues::dialogue-node
                              :state initial-statement))
         (queue (make-initial-queue node
                                    :queueing-function #'enqueue-at-end))
         (problem (make-instance 'dialogue-search-problem
                                 :formula initial-statement
                                 :ruleset rules)))
    (bounded-bfs-with-nodes problem depth queue)))

(defun opponent-node-p (node)
  (or (root-node-p node)
      (proponent-node-p (parent node))))

(defun proponent-node-p (node)
  (and (not (root-node-p node))
       (opponent-node-p (parent node))))

(defun opponent-assertions-by-occurrence (node)
  "A list of all formulas asserted by Opponent in the sequence of moves up to NODE.  All occurrences of otherwise identical formulas are listed (thus, the \"same\" formula may appear more than once in the resulting list)."
  (labels ((oabo (node)
             (cond ((root-node-p node)
                    nil)
                   ((proponent-node-p node)
                    (cons (statement (action node))
                          (oabo (parent node))))
                   (t
                    (oabo (parent node))))))
    (remove-duplicates (oabo node) :test #'eq)))

(defun opponent-assertions (node)
  "A list of all formulas asserted by Opponent in the sequence of moves up to NODE.  The list is given up to formula equality; distinct occurrences of otherwise identical formulas are not considered."
  (remove-duplicates (opponent-assertions-by-occurrence node)
                     :test #'equal-formulas?))

(defun opponent-attacked-formulas-by-occurrence (node)
  "A list of formulas attacked so far by Opponent in the dialogue leading up to NODE.  Distinct occurrences of the same formula will appear here (thus, the \"same\" formula may appear more than once in this list)."
  (labels ((oafbo (node)
             (cond ((root-node-p node)
                    (list (state node)))
                   ((proponent-node-p node)
                    (let ((move (action node)))
                      (if (attack-p move)
                          (cons (reference move)
                                (oafbo (parent node)))
                          (oafbo (parent node)))))
                   (t
                    (oafbo (parent node))))))
    (remove-duplicates (oafbo node) :test #'eq)))

(defun opponent-attacked-formulas (node)
  "A list of formulas attacked so far by Opponent in the dialogue leading up to NODE.  The list is given up to formula equality (thus, if Opponent attacks distinct instances of the same formula, only one of the occurrences will appear in this list.)"
  (remove-duplicates (opponent-attacked-formulas-by-occurrence node)
                     :test #'equal-formulas?))

(defun proponent-wins? (node concessions)
  "Does Proponent win the dialogue ending at NODE?"
  (when (proponent-node-p node)
    (let ((opponent-assertions (opponent-assertions node))
          (opponent-attacks (opponent-attacked-formulas node)))
      (or (find-if #'falsum-p concessions)
          (find-if #'falsum-p opponent-assertions)
          (intersection opponent-attacks
                        concessions
                        :test #'equal-formulas?)
          (intersection opponent-attacks
                        opponent-assertions
                        :test #'equal-formulas?)))))

(defmethod goal-test ((problem dialogue-search-problem)
                      (node dialogue-node))
  (proponent-wins? node (concessions problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dialogue-strategy-search-problem (problem)
  ((rules
    :initform nil
    :type list
    :accessor rule)))

(defun strategy-successors (strategy rules)
  (declare (ignore strategy rules))
  nil)

(defun some-non-cutoff-result (pred list)
  (if (null list)
      nil
      (loop
	 :with all-are-cutoffs = t
	 :for search-result :in list
	 :do
         (let ((result (funcall pred search-result)))
           (setf all-are-cutoffs (and all-are-cutoffs
                                      (eq result :cutoff)))
           (unless (eq result :cutoff)
             (unless (null result)
               (return t))))
	 :finally
         (if all-are-cutoffs
             (return :cutoff)
             (return nil)))))

(defun every-disallowing-cutoffs (pred list)
  (loop
     :for elt :in list
     :for result = (funcall pred elt)
     :when (eq result :cutoff) :do (return :cutoff)
     :when (null result) :do (return nil)
     :finally (return t)))

(defun proponent-has-winning-strategy? (dialogue cutoff)
  (cond ((minusp cutoff)
         :cutoff)
	((zerop cutoff)
         (or (null (next-opponent-moves dialogue))
             :cutoff))
	(t (let ((every-result
		  (every-disallowing-cutoffs
		   #'(lambda (opponent-move)
		       (let ((dialogue-opponent
			      (add-move-to-dialogue dialogue
                                                    opponent-move)))
			 (some-non-cutoff-result
			  #'(lambda (proponent-move)
			      (let ((dialogue-proponent
				     (add-move-to-dialogue dialogue-opponent
							   proponent-move)))
				(proponent-has-winning-strategy? dialogue-proponent
								 (- cutoff 2))))
			  (next-proponent-moves dialogue-opponent))))
		   (next-opponent-moves dialogue))))
	     (if (eq every-result :cutoff)
		 :cutoff
		 every-result)))))

(defgeneric intuitionistically-valid? (formula strategy-depth)
  (:documentation "Does there exist a winning strategy for FORMULA having depth at most STRATEGY-DEPTH?"))

(defmethod intuitionistically-valid? ((formula atomic-formula) strategy-depth)
  nil)

(defun tptp->dialogue (db ruleset)
  (let ((c (conjecture-formula db))
        (premises (non-conjecture-formulas db)))
    (setf c (formula c))
    (setf premises (mapcar #'formula premises))
    (setf c (equivalence->conjunction c))
    (cond ((atomic-formula-p c)
           (if (null premises)
               (error "Cannot make a dialogue for a TPTP database having only an atomic conjecture formula.")
               (make-instance 'dialogue
                              :initial-formula (make-implication (binarize (apply #'make-multiple-arity-conjunction premises)) c)
                              :ruleset ruleset)))
          ((null premises)
           (make-instance 'dialogue
                          :initial-formula c
                          :ruleset ruleset))
          ((length= 1 premises)
           (let* ((premise (first premises))
                  (initial-formula (make-implication premise c))
                  (dialogue (make-instance 'dialogue
                                           :initial-formula initial-formula
                                           :ruleset ruleset))
                  (i 0))
             (setf dialogue
                   (add-move-to-dialogue dialogue
                                         (make-instance 'opponent-move
                                                        :attack t
                                                        :reference 0
                                                        :statement premise)))
             (incf i)
             (add-move-to-dialogue dialogue
                                   (make-instance 'proponent-move
                                                  :attack nil
                                                  :reference 1
                                                  :statement c))))
          (t
           (let* ((conjunction (binarize (apply #'make-multiple-arity-conjunction premises)))
                  (initial-formula (make-implication conjunction c))
                  (dialogue (make-instance 'dialogue
                                           :initial-formula initial-formula
                                           :ruleset ruleset))
                  (i 0)
                  (lhs (lhs conjunction))
                  (rhs (rhs conjunction)))
             (setf dialogue
                   (add-move-to-dialogue dialogue
                                         (make-instance 'opponent-move
                                                        :attack t
                                                        :reference 0
                                                        :statement conjunction)))
             (incf i)
             (while (and premises (rest premises))
               (let ((rest (rest premises)))
                 (setf dialogue
                       (add-move-to-dialogue dialogue
                                             (make-instance 'proponent-move
                                                            :attack t
                                                            :reference i
                                                            :statement *attack-left-conjunct*)))
                 (incf i)
                 (setf dialogue
                       (add-move-to-dialogue dialogue
                                             (make-instance 'opponent-move
                                                            :attack nil
                                                            :reference i
                                                            :statement lhs)))
                 (incf i)
                 (setf dialogue
                       (add-move-to-dialogue dialogue
                                             (make-instance 'proponent-move
                                                            :attack t
                                                            :reference (- i 2)
                                                            :statement *attack-right-conjunct*)))
                 (incf i)
                 (setf dialogue
                       (add-move-to-dialogue dialogue
                                             (make-instance 'opponent-move
                                                            :attack nil
                                                            :reference i
                                                            :statement rhs)))
                 (incf i)
                 (setf lhs (second premises))
                 (setf rhs (third premises))
                 (setf premises rest)))
             (setf dialogue (add-move-to-dialogue dialogue
                                                  (make-instance 'proponent-move
                                                                 :attack nil
                                                                 :reference 1
                                                                 :statement c))))))))

(defmethod intuitionistically-valid? ((tptp pathname) strategy-depth)
  (intuitionistically-valid? (parse-tptp tptp) strategy-depth))

(defmethod intuitionistically-valid? ((db tptp-db) strategy-depth)
  (if (contains-quantifier-p db)
      (loop
         :for term-depth = 0
         :for ruleset = (make-instance 'ruleset
                                       :description (format nil "E, maximum term depth = ~d" term-depth)
                                       :expander #'(lambda (dialogue)
                                                     (e-fol-expander--no-repetitions+prefer-defenses dialogue term-depth))
                                       :validator #'e-fol-validator)
         :for dialogue = (tptp->dialogue db ruleset)
         :for search-result = (proponent-has-winning-strategy? dialogue strategy-depth)
         :do
         (when search-result
           (unless (eql search-result :cutoff)
             (return t))))
      (let ((dialogue-1 (tptp->dialogue db *e-ruleset--no-repetitions*))
            (dialogue-2 (tptp->dialogue db *e-ruleset--prefer-defenses*))
            (dialogue-3 (tptp->dialogue db *e-ruleset*)))
        (let ((search-result-1 (proponent-has-winning-strategy? dialogue-1 strategy-depth)))
          (cond ((eql search-result-1 :cutoff)
                 (let ((search-result-2 (proponent-has-winning-strategy? dialogue-2 strategy-depth)))
                   (cond ((eql search-result-2 :cutoff)
                          (proponent-has-winning-strategy? dialogue-3 strategy-depth))
                         (search-result-2
                          t)
                         (t
                          (proponent-has-winning-strategy? dialogue-3 strategy-depth)))))
                (search-result-1
                 t)
                (t
                 (let ((search-result-2 (proponent-has-winning-strategy? dialogue-2 strategy-depth)))
                   (cond ((eql search-result-2 :cutoff)
                          (proponent-has-winning-strategy? dialogue-3 strategy-depth))
                         (search-result-2
                          t)
                         (t
                          (proponent-has-winning-strategy? dialogue-3 strategy-depth))))))))))

(defmethod intuitionistically-valid? ((formula formula) strategy-depth)
  (let* ((n (make-instance 'dialogues::node :state formula))
         (problem (make-instance 'strategy-search-problem
                                 :initial-node n
                                 :ruleset *e-ruleset*)))
    (general-bounded-search problem #'enqueue-at-end strategy-depth)))

(defun develop-dialogue-tree-to-depth (tree-root depth problem)
  (let ((expandable-leaves (expandable-leaf-nodes tree-root)))
    (dolist (leaf expandable-leaves tree-root)
      (exhaustive-depth-limited-search problem depth leaf))))

(defun dialogue->search-tree (dialogue)
  "Construct a search tree (a sequence, in fact) from DIALOGUE."
  (let (nodes)
    (dotimes (i (dialogue-length dialogue)) ; first construct the nodes
      (push (make-instance 'dialogues::node
                           :state (truncate-dialogue dialogue i)
                           :parent nil
                           :action nil
                           :successors nil
                           :depth i)
	    nodes))
    (setf nodes (nreverse nodes))
    (loop
       :for parent :in nodes
       :for node-successor :in (cdr nodes)
       :do
	 (setf (parent node-successor) parent
	       (successors parent) (list node-successor)))
    (car (last nodes))))

(defmethod proponent-node? ((node node))
  (proponent-move-p (last-move (state node))))

(defmethod opponent-node? ((node node))
  (opponent-move-p (last-move (state node))))

(defun proponent-ws-from-opponent-node (opponent-node &optional ruleset)
  "Find a winning strategy from OPPONENT-NODE, which is supposed to represent a move just played by opponent. Return :DIALOGUE-TREE-TOO-SHALLOW if there are any unexpanded nodes that, if expanded, could make a difference in the determination of the existence of a winning strategy.  Return NIL if there are no winning strategies for Proponent starting from OPPONENT-NODE.  If there are, return a copy of OPPONENT-NODE that contains the winning strategy.  It is assumed that OPPONENT-NODE is expanded."
  (let ((opponent-succs (successors opponent-node)))
    (if (expanded-p opponent-node)
        (if (null opponent-succs)
            nil
            (let ((strategies (mapcar #'(lambda (node)
                                          (proponent-ws-from-proponent-node node ruleset))
                                      opponent-succs)))
              (if (every #'null strategies)
                  nil
                  (let ((maybe-winner (find-if #'node-p strategies)))
                    (if (node-p maybe-winner)
                        (make-instance 'dialogues::node
                                       :state (state opponent-node)
                                       :parent (parent opponent-node)
                                       :action (action opponent-node)
                                       :successors (list maybe-winner)
                                       :depth (depth opponent-node))
                        :dialogue-tree-too-shallow)))))
        :dialogue-tree-too-shallow)))

(defun proponent-ws-from-proponent-node (proponent-node &optional ruleset)
  (if (expanded-p proponent-node)
      (let ((succs (successors proponent-node)))
	(let ((strategies (mapcar #'(lambda (node)
				      (proponent-ws-from-opponent-node node ruleset))
				  succs)))
	  (if (member nil strategies)
	      nil
	      (if (member :dialogue-tree-too-shallow strategies)
		  :dialogue-tree-too-shallow
		  (make-instance 'dialogues::node
                                 :state (state proponent-node)
                                 :parent (parent proponent-node)
                                 :action (action proponent-node)
                                 :successors strategies
                                 :depth (depth proponent-node))))))
      :dialogue-tree-too-shallow))

;;; dialogue-search.lisp ends here
