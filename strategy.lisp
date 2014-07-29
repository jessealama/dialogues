;;; strategy.lisp Working with dialogue strategies

(in-package :dialogues)

(defclass dialogue-node (node)
  nil
  (:documentation "A node in the search for a (Proponent) winning strategy."))

(defmethod initialize-instance :after ((node dialogue-node) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (and (slot-boundp node 'action)
             (not (slot-boundp node 'state)))
    (setf (state node)
          (statement (action node)))))

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

(defun contains-winning-strategy-p (node)
  "Does NODE contain a winning strategy (for Proponent)?"
  (declare (ignore node))
  nil)

(defmethod goal-test ((problem strategy-search-problem) (node dialogue-node))
  (contains-winning-strategy-p (root-of node)))

(defmethod successors-in-problem ((problem strategy-search-problem)
                                  (node dialogue-node))
  nil)

;;; strategy.lisp ends here
