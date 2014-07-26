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

(defun opponent-assertions-by-occurrence (node)
  "A list of all formulas asserted by Opponent in the sequence of moves up to NODE.  All occurrences of otherwise identical formulas are listed (thus, the \"same\" formula may appear more than once in the resulting list)."
  (declare (ignore node))
  nil)

(defun opponent-assertions (node)
  "A list of all formulas asserted by Opponent in the sequence of moves up to NODE.  The list is given up to formula equality; distinct occurrences of otherwise identical formulas are not considered."
  (remove-if-not #'equal-formulas?
                  (opponent-assertions-by-occurrence node)))

(defun opponent-attacked-formulas-by-occurrence (node)
  "A list of formulas attacked so far by Opponent in the dialogue leading up to NODE.  Distinct occurrences of the same formula will appear here (thus, the \"same\" formula may appear more than once in this list)."
  (declare (ignore node))
  nil)

(defun opponent-attacked-formulas (node)
  "A list of formulas attacked so far by Opponent in the dialogue leading up to NODE.  The list is given up to formula equality (thus, if Opponent attacks distinct instances of the same formula, only one of the occurrences will appear in this list.)"
  (remove-if-not #'equal-formulas?
                 (opponent-attacked-formulas-by-occurrence node)))

(defun proponent-wins-p (node)
  "Does Proponent win the dialogue ending at NODE?"
  (when (proponent-node-p node)
    (let ((opponent-assertions (opponent-assertions node))
          (opponent-attacks (opponent-attacked-formulas node)))
      (or (find-if #'falsum-p opponent-assertions)
          (not (null (intersection opponent-attacks
                                   opponent-assertions
                                   :test #'equal-formulas?)))))))

(defun contains-winning-strategy-p (node)
  "Does NODE contain a winning strategy (for Proponent)?"
  (declare (ignore node))
  nil)

(defmethod goal-test ((problem dialogue-search-problem) (node node))
  (declare (ignore problem))
  (proponent-wins-p node))

(defmethod goal-test ((problem strategy-search-problem) (node dialogue-node))
  (contains-winning-strategy-p (root-of node)))

(defmethod successors-in-problem ((problem strategy-search-problem)
                                  (node dialogue-node))
  nil)

;;; strategy.lisp ends here
