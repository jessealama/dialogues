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

(defun proponent-wins-p (node)
  "Does Proponent win the dialogue ending at NODE?"
  nil)

(defun contains-winning-strategy-p (node)
  "Does NODE contain a winning strategy (for Proponent)?"
  (when (expanded-p node)
    (cond ((opponent-node-p node)
           (every #'contains-winning-strategy-p (successors node)))
          ((proponent-node-p node)
           (some #'contains-winning-strategy-p (successors node))))))

(defmethod goal-test ((problem strategy-search-problem) (node node))
  (contains-winning-strategy-p (root-of node)))

(defmethod successors-in-problem ((problem strategy-search-problem)
                                  (node node))
  nil)

;;; strategy.lisp ends here
