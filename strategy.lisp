;;; strategy.lisp Working with dialogue strategies

(in-package :dialogues)

(defclass strategy-search-problem (problem)
  ((ruleset
    :type ruleset
    :reader ruleset
    :initarg :ruleset
    :documentation "The ruleset according to which dialogues (branches in the search tree) are supposed to adhere.")
   (initial-formula
    :reader initial-formula
    :type formula
    :initarg :initial-formula))
  (:documentation "Search for a (Proponent) winning strategy."))

(defclass dialogue-node (node)
  nil
  (:documentation "A node in the search for a (Proponent) winning strategy."))

(defun opponent-node-p (node)
  (or (root-node-p node)
      (proponent-node-p (parent node))))

(defun proponent-node-p (node)
  (and (not (root-node-p node))
       (opponent-node-p (parent node))))

(defun contains-winning-strategy-p (node)
  "Does NODE contain a winning strategy (for Proponent)?"
  (when (expanded-p node)
    (cond ((opponent-node-p node)
           (every #'contains-winning-strategy-p (successors node)))
          ((proponent-node-p node)
           (some #'contains-winning-strategy-p (successors node))))))

(defmethod goal-test ((problem strategy-search-problem) (node node))
  (contains-winning-strategy-p (root-of node)))

(defmethod successors-in-problem ((problem strategy-search-problem) (node node))
  nil)

(defmethod successors-in-problem ((problem strategy-search-problem)
                                  (node node))
  (cond ((opponent-node-p node)
         (cond ((root-node-p node)
                (let ((f (initial-formula problem)))
                  (cond ((atomic-formula-p f)
                         (list (cons :attack *attack-atom*)))
                        (t
                         (error "How should Opponent attack the initial formula~%~%  ~a~%~%?" f)))))
               (t
                (destructuring-bind (attack-or-defend . statement)
                    (action node)
                  (assert (or (eql attack-or-defend :attack)
                              (eql attack-or-defend :defend))
                          attack-or-defend
                          "Action should be either :attack or :defend, but we are dealing with~%~%  ~a~%" attack-or-defend)
                  (when (eql attack-or-defend :defend)
                    ;; Opponent must attack the statement
                    (cond ((atomic-formula-p statement)
                           (list (cons :attack *attack-atom*)))
                          (t
                           (error "How should Opponent attack the formula~%~%  ~a~%~%?" statement))))
                  (when (eql attack-or-defend :attack)
                    ;; attacks
                    (attack)
                    ;; defenses

                    ))
                ;; attacks
                ;; defenses
                )))))

;;; strategy.lisp ends here
