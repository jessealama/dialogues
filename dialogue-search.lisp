;;; dialogue-search.lisp Dialogue games as search trees

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'utils "utils.lisp")
  (require 'dialogues "dialogues.lisp")
  (require 'search "search.lisp"))

(defstruct (dialogue-search-problem
	     (:include problem))
  (rules nil :type list)
  (signature nil :type signature))

(defmethod successors ((dsp dialogue-search-problem) node)
  (let ((dialogue-so-far (node-state node))
	(rules (dialogue-search-problem-rules dsp)))
    (let (extensions)
      (dolist (player '(p o) extensions)
	(dolist (stance '(a d))
	  (let ((next-moves (next-moves dialogue-so-far rules player stance)))
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
		    extensions)))))))

(defmethod goal-test ((dsp dialogue-search-problem) node)
  (let ((successors (successors dsp node)))
    (setf (node-successors node) successors)
    (null successors)))

(defun dialogue-search (rules initial-statement signature)
  (if (formula? initial-statement signature)
      (let* ((initial-state (make-dialogue initial-statement signature))
	     (problem (make-dialogue-search-problem :initial-state initial-state
						    :signature signature
						    :rules rules)))
	(breadth-first-search problem))
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

(provide 'dialogue-search)

;;; dialogue-search.lisp ends here