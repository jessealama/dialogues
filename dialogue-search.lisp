;;; dialogue-search.lisp Dialogue games as search trees

(in-package :dialogues)

(defstruct (dialogue-search-problem
	     (:include problem))
  (rules nil :type ruleset)
  (signature nil :type signature))

(defmethod goal-test ((problem dialogue-search-problem) node)
  (let ((game (node-state node)))
    (and (proponent-move? (last-move game))
	 (null (all-next-opponent-moves-at-position game 
						    (dialogue-length game))))))

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

(defun dialogue-search-bfs (rules initial-statement signature &optional more-nodes)
  (if (belongs-to-signature? signature initial-statement)
      (let* ((initial-state (make-dialogue initial-statement signature rules))
	     (problem (make-dialogue-search-problem :initial-state initial-state
						    :signature signature
						    :rules rules)))
	(breadth-first-search-for-bottom-with-nodes problem more-nodes))
      (error "The initial statement ~A is not a formula according to the given signature ~A" initial-statement signature)))

(defun dialogue-search-dfs (rules initial-statement signature)
  (if (belongs-to-signature? signature initial-statement)
      (let* ((initial-state (make-dialogue initial-statement signature rules))
	     (problem (make-dialogue-search-problem :initial-state initial-state
						    :signature signature
						    :rules rules)))
	(depth-first-search-for-bottom problem))
      (error "The initial statement ~A is not a formula according to the given signature ~A" initial-statement signature)))

(defun dialogue-search-dfs-no-cycles (rules initial-statement signature)
  (if (belongs-to-signature? signature initial-statement)
      (let* ((initial-state (make-dialogue initial-statement signature rules))
	     (problem (make-dialogue-search-problem :initial-state initial-state
						    :signature signature
						    :rules rules)))
	(no-cycles-depth-first-search-for-bottom problem #'equal-dialogues?))
      (error "The initial statement ~A is not a formula according to the given signature ~A" initial-statement signature)))

(defun bounded-dialogue-search (rules initial-statement signature depth)
  (if (belongs-to-signature? signature initial-statement)
      (let* ((initial-state (make-dialogue initial-statement signature rules))
	     (problem (make-dialogue-search-problem :initial-state initial-state
						    :signature signature
						    :rules rules)))
	(depth-limited-search problem depth))
      (error "The initial statement ~A is not a formula according to the given signature ~A" initial-statement signature)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching for strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (dialogue-strategy-search-problem
	     (:include problem))
  (rules nil :type list)
  (signature nil :type signature))

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

(defun dialogue-valid? (rules signature statement depth)
  "Determine whether statement is valid in the dialogue games defined by
RULES.  \"Valid\" means that Proponent has a winning strategy in the
game tree developed down to depth DEPTH."
  (let ((dialogue (make-dialogue statement signature rules)))
    (multiple-value-bind (ok? violators)
	(eval-entire-dialogue dialogue)
      (declare (ignore violators))
      (if ok?
	  (proponent-has-winning-strategy? dialogue depth 1)
	  (error 'inappropriate-initial-statement-error
		 :rules rules
		 :statement statement)))))

(defun classify-formulas (formulas rules signature search-depth)
  (let (inappropriate valid invalid cutoff)
    (dolist (formula formulas)
      (handler-case (let ((result (dialogue-valid? rules
						   signature
						   formula
						   search-depth)))
		      (ecase result
			(:cutoff (push formula cutoff))
			(t (push formula valid))
			((nil) (push formula invalid))))
	(inappropriate-initial-statement-error ()
	  (push formula inappropriate))))
    (values inappropriate valid invalid cutoff)))

(defun classify-all-formulas (rules signature formula-depth search-depth)
  (classify-formulas (reduced-enumeration formula-depth signature)
		     rules
		     signature
		     search-depth))

;;; Strategy search for specific rulesets

(defun ncl-valid? (formula depth signature)
  (dialogue-valid? nearly-classical-dialogue-rules
		   signature
		   formula
		   depth))

;;; dialogue-search.lisp ends here