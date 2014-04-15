(in-package :dialogues)

(defclass ruleset ()
  ((expander
    :type function
    :initform (error "A ruleset needs an expander function.")
    :initarg :expander
    :documentation "A unary function that takes a dialogue and returns a list of moves by which the given dialogue can be continued.")))

(defgeneric defend-against (statement attack)
  (:documentation "Defend STATEMENT against ATTACK."))

(defmethod defend-against ((statement t) (attack t))
  (error "Don't know how to defend~%~%  ~a~%~%against the attack~%~%  ~a~%" statement attack))

(defmethod defend-against ((statement implication) (attack t))
  (consequent statement))

(defmethod defend-against ((move move) attack)
  (defend-against (statement move) attack))

(defun e-propositional-expander (dialogue)
  (let ((initial (initial-formula dialogue))
        (l (dialogue-length dialogue)))
    (cond ((zerop l)
           (list (make-instance 'opponent-move
                                :statement (defend-against initial nil)
                                :reference 0
                                :attack t)))
          (t
           ;; Generate all possible defenses.  Rules: (1) an
           ;; attack may be defended only once; (2) P may not assert
           ;; an atom before O; (3) only the most recent open attack
           ;; may be defended
           (let ((responses nil))
             (let ((most-recent (most-recent-open-attack dialogue)))
               (when (and (integerp most-recent))
                 (let* ((attack (nth-move dialogue most-recent))
                        (attack-reference (reference attack))
                        (attack-statement (statement attack))
                        (attacked-statement (nth-statement dialogue attack-reference))
                        (defense (defend-against attacked-statement attack-statement)))
                   ;; P can assert an atom only if O has already asserted it
                   (if (oddp l)
                       (when (atomic-formula-p defense)
                         (when (member defense (opponent-assertions dialogue))
                           (push (make-instance 'proponent-move
                                                :reference most-recent
                                                :statement defense
                                                :attack nil)
                                 responses)))
                       (push (make-instance 'opponent-move
                                            :reference most-recent
                                            :statement defense
                                            :attack nil)
                             responses)))))
             ;; all possible attacks -- TODO
             responses)))))