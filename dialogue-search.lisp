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
    :initarg :ruleset)))

(defmethod goal-test ((problem dialogue-search-problem) node)
  (expand node problem)
  (and (null (node-successors node))
       (proponent-move-p (last-move (node-state node)))))

(defmethod successors ((dsp dialogue-search-problem) node)
  (continuations (node-state node)))

(defun dialogue-search-bfs (rules initial-statement &optional more-nodes)
  (let* ((initial-state (make-instance 'dialogue
                                       :initial-formula initial-statement
                                       :rulset rules))
         (problem (make-instance 'dialogue-search-problem
                                 :initial-state initial-state
                                 :rules rules)))
    (breadth-first-search-for-bottom-with-nodes problem more-nodes)))

(defun dialogue-search-dfs (rules initial-statement)
  (let* ((initial-state (make-instance 'dialogue
                                       :initial-formula initial-statement
                                       :ruleset rules))
         (problem (make-instance 'dialogue-search-problem
                                 :initial-state initial-state
                                 :rules rules)))
    (depth-first-search-for-bottom problem)))

;; (defun dialogue-search-dfs-no-cycles (rules initial-statement)
;;   (let* ((initial-state (make-instance 'dialogue
;;                                        :initial-formula initial-statement
;;                                        :ruleset rules))
;;          (problem (make-dialogue-search-problem :initial-state initial-state
;;                                                 :rules rules)))
;;     (no-cycles-depth-first-search-for-bottom problem #'equal-dialogues?)))

(defun bounded-dialogue-search-dfs (rules initial-statement depth &optional (initial-state (make-instance 'dialogue :initial-formula initial-statement :ruleset rules)))
  (let ((problem (make-instance 'dialogue-search-problem
                                :initial-state initial-state
                                :rules rules)))
    (depth-limited-dfs-search problem depth)))

(defun bounded-dialogue-search-bfs (rules initial-statement depth
				    &optional (initial-state (make-instance 'dialogue :initial-formula initial-statement :ruleset rules))
				              initial-queue)
  (let ((problem (make-instance 'dialogue-search-problem
                                :initial-state initial-state
                                :rules rules)))
	(bounded-breadth-first-search-with-nodes problem depth initial-queue)))

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

;; (defun dialogue-valid? (rules statement depth)
;;   "Determine whether statement is valid in the dialogue games defined by
;; RULES.  \"Valid\" means that Proponent has a winning strategy in the
;; game tree developed down to depth DEPTH."
;;   (let ((dialogue (make-dialogue statement rules)))
;;     (if (fast-eval-entire-dialogue dialogue)
;; 	(proponent-has-winning-strategy? dialogue depth 1)
;; 	(error 'inappropriate-initial-statement-error
;; 	       :rules rules
;; 	       :statement statement))))

;; (defun classify-formulas (formulas rules search-depth)
;;   (let (inappropriate valid invalid cutoff)
;;     (dolist (formula formulas)
;;       (handler-case (let ((result (dialogue-valid? rules
;; 						   formula
;; 						   search-depth)))
;; 		      (ecase result
;; 			(:cutoff (push formula cutoff))
;; 			(t (push formula valid))
;; 			((nil) (push formula invalid))))
;; 	(inappropriate-initial-statement-error ()
;; 	  (push formula inappropriate))))
;;     (values inappropriate valid invalid cutoff)))

;; Functionality dropped.  But it might be nice to restore it later.
;;
;; (defun classify-all-formulas (rules signature formula-depth search-depth)
;;   (classify-formulas (reduced-enumeration formula-depth signature)
;; 		     rules
;; 		     signature
;; 		     search-depth))

;;; Strategy search for specific rulesets

;; (defun defenses--e (strategy-node)
;;   (unless (attack-p strategy-node)
;;     (error "The move of a strategy node is not an attack, so how can we defend against it?"))
;;   (let ((dialogue (strategy-node->dialogue strategy-node)))
;;     (let (()))))

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
      (let ((dialogue (tptp->dialogue db *e-ruleset--no-repetitions*)))
        (let ((search-result (proponent-has-winning-strategy? dialogue strategy-depth)))
          (when search-result
            (not (eql search-result :cutoff)))))))

(defmethod intuitionistically-valid? ((formula formula) strategy-depth)
  (if (contains-quantifier-p formula)
      (loop
         :for term-depth = 0
         :for ruleset = (make-instance 'ruleset
                                       :description (format nil "E, maximum term depth = ~d" term-depth)
                                       :expander #'(lambda (dialogue)
                                                     (e-fol-expander--no-repetitions+prefer-defenses dialogue term-depth)))
         :for dialogue = (make-instance 'dialogue
                                        :initial-formula formula
                                        :ruleset ruleset)
         :for search-result = (proponent-has-winning-strategy? dialogue strategy-depth)
         :do
         (when search-result
           (unless (eql search-result :cutoff)
             (return t))))
      (let ((dialogue (make-instance 'dialogue
                                     :initial-formula formula
                                     :ruleset *e-ruleset--no-repetitions*)))
        (proponent-has-winning-strategy? dialogue strategy-depth))))

(defun develop-dialogue-tree-to-depth (tree-root depth problem)
  (let ((expandable-leaves (expandable-leaf-nodes tree-root)))
    (dolist (leaf expandable-leaves tree-root)
      (exhaustive-depth-limited-search problem depth leaf))))

;; (defun dialogue-search-tree (formula rules depth)
;;   (let ((earlier-entries (search-trees-for-formula-with-rules formula rules))
;; 	(problem (make-dialogue-search-problem :rules rules
;;                                                :initial-state (make-dialogue formula

;;                                                                              rules))))
;;     (let ((tree (loop
;; 		   for entry in earlier-entries
;; 		   do
;; 		     (destructuring-bind (ruleset d root)
;; 			 entry
;; 		       (when (equal-rulesets? ruleset rules)
;; 			 (when (>= d depth)
;; 			   (return root))))
;; 		   finally
;; 		     (return (create-start-node problem)))))
;;       (cond ((>= (node-depth tree) depth)
;; 	     tree)
;; 	    (t
;; 	     (let ((new-tree (develop-dialogue-tree-to-depth tree depth problem)))
;; 	       (register-dialogue-tree formula rules depth new-tree)
;; 	       new-tree))))))

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
  (proponent-move-p (last-move (node-state node))))

(defmethod opponent-node? ((node node))
  (opponent-move-p (last-move (node-state node))))

(defun proponent-ws-from-opponent-node (opponent-node &optional ruleset)
  "Find a winning strategy from OPPONENT-NODE, which is supposed to
represent a move just played by opponent.
Return :DIALOGUE-TREE-TOO-SHALLOW if there are any unexpanded nodes that,
if expanded, could make a difference in the determination of the
existence of a winning strategy.  Return NIL if there are no winning
strategies for Proponent starting from OPPONENT-NODE.  If there are,
return a copy of OPPONENT-NODE that contains the winning strategy.  It
is assumed that OPPONENT-NODE is expanded."
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
        :dialogue-tree-too-shallow)))

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

;; (defun winning-strategy (formula ruleset depth &optional dialogue)
;;   (let* ((problem (make-dialogue-search-problem :rules ruleset
;; 						:initial-state (or dialogue
;; 								   (make-dialogue formula
;; 										  ruleset))))
;; 	 (root (develop-dialogue-tree-to-depth (create-start-node problem) depth problem)))
;;     (proponent-ws-from-proponent-node root ruleset)))

(defun explain-strategy (winning-strategy)
  (declare (ignore winning-strategy))
  (format t "implement something sensible here"))


;;; dialogue-search.lisp ends here
