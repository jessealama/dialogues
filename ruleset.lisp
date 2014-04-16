(in-package :dialogues)

(defclass ruleset ()
  ((expander
    :type function
    :accessor expander
    :initform (error "A ruleset needs an expander function.")
    :initarg :expander
    :documentation "A unary function that takes a dialogue and returns a list of moves by which the given dialogue can be continued.")
   (description
    :type string
    :accessor description
    :initform (error "A ruleset needs a description.")
    :initarg :description
    :documentation "A description of the ruleset.")))

(defmethod print-object ((r ruleset) stream)
  (print-unreadable-object (r stream :type t :identity nil)
    (format stream "~a" (description r))))

(defun opponent-asserted-atom-earlier? (dialogue move)
  (when (atomic-formula-p move)
    (member (statement move)
            (opponent-assertions dialogue))))

(defun attacks (dialogue)
  (remove-if-not #'attack-p (plays dialogue)))

(defun opponent-attacks (dialogue)
  (remove-if-not #'opponent-move-p (attacks dialogue)))

(defun duplicate-opponent-attack? (dialogue move)
  (when (opponent-move-p move)
    (when (attack-p move)
      (member (reference move)
              (opponent-attacks dialogue)
              :key #'reference
              :test #'=))))

(defgeneric possible-propositional-attacks (formula)
  (:documentation "The possible attacks against FORMULA, regarded as a part of proposition logic.  (Any generalizations in FORMULA are treated as if they were atoms.)"))

(defmethod possible-propositional-attacks ((x t))
  (error "How to generate a list of possible propositional attacks against~%~%  ~a~%~%?" x))

(defmethod possible-propositional-attacks ((f atomic-formula))
  nil)

(defmethod possible-propositional-attacks ((f binary-conjunction))
  (list *attack-left-conjunct* *attack-right-conjunct*))

(defmethod possible-propositional-attacks ((f binary-disjunction))
  (list *which-disjunct?*))

(defmethod possible-propositional-attacks ((f implication))
  (list (antecedent f)))

(defmethod possible-propositional-attacks ((g generalization))
  nil)

(defmethod possible-propositional-attacks ((n negation))
  (list (unnegate n)))

(defun d-propositional-expander (dialogue)
  ;; Rules: (1) an attack may be defended only once; (2) P may not
  ;; assert an atom before O; (3) only the most recent open attack may
  ;; be defended.
  (let ((initial (initial-formula dialogue))
        (plays (plays dialogue))
        (last-move (last-move dialogue))
        (responses nil))
    (cond ((null plays)
           (push (make-instance 'opponent-move
                                :statement (defend-against initial nil)
                                :reference 0
                                :attack t)
                 responses))
          (t
           ;; Generate all possible defenses.
           (let ((most-recent (most-recent-open-attack dialogue))
                 (other-class (if (opponent-move-p last-move)
                                  'proponent-move
                                  'opponent-move)))
             (when (integerp most-recent)
               (let* ((attack (nth-move dialogue most-recent))
                      (attack-reference (reference attack))
                      (attack-statement (statement attack))
                      (attacked-statement (nth-statement dialogue attack-reference)))
                 (let ((defense (cond ((implication-p attacked-statement)
                                       (consequent attacked-statement))
                                      ((binary-conjunction-p attacked-statement)
                                       (cond ((eql attack-statement *attack-left-conjunct*)
                                              (lhs attacked-statement))
                                             ((eql attack-statement *attack-right-conjunct*)
                                              (rhs attack-statement))
                                             (t
                                              (error "The statement~%~%  ~a~%~%was attacked by~%~%  ~a~%~%which is neither ~a nor ~a as we expect." attacked-statement attack-statement *attack-left-conjunct* *attack-right-conjunct*))))

                                      (t
                                       (error "How to defend~%~%  ~a~%~%?~%" attacked-statement)))))
                   (push (make-instance other-class
                                        :reference most-recent
                                        :statement defense
                                        :attack nil)
                         responses)))))
           ;; all possible attacks
           (let ((other-class (if (opponent-move-p last-move)
                                  'proponent-move
                                  'opponent-move))
                 (start (if (opponent-move-p last-move) 1 2)))
             (loop
                :with l = (dialogue-length dialogue)
                :for i :from start :upto l :by 2
                :for move = (nth-move dialogue i)
                :for statement = (statement move)
                :when (and (formula-p statement)
                           (not (atomic-formula-p statement)))
                :do
                (cond ((implication-p statement)
                       (push (make-instance other-class
                                            :attack t
                                            :reference i
                                            :statement (antecedent statement))
                             responses))
                      ((binary-conjunction-p statement)
                       (push (make-instance other-class
                                            :attack t
                                            :reference i
                                            :statement *attack-left-conjunct*)
                             responses)
                       (push (make-instance other-class
                                            :attack t
                                            :reference i
                                            :statement *attack-right-conjunct*)
                             responses))

                      (t
                       (error "How to attack~%~%  ~a~%~%?~%" statement)))))
           ;; filter out illegal assertions by P of atoms
           (when (opponent-move-p last-move)
             (setf responses
                   (remove-if #'(lambda (move)
                                  (and (atomic-formula-p move)
                                       (not (opponent-asserted-atom-earlier? dialogue move))))
                                  responses)))
           ;; filter out duplicate Opponent attacks
           (when (proponent-move-p last-move)
             (setf responses
                   (remove-if-not #'(lambda (move)
                                      (duplicate-opponent-attack? dialogue move))
                                  responses)))))
    responses))

(defun e-propositional-expander (dialogue)
  ;; Rules: E rules, with the restriction that Opponent must respond immediately to Proponent.
  (let ((last-move (last-move dialogue))
        (l (dialogue-length dialogue))
        (d-possibilities (d-propositional-expander dialogue)))
    (if (proponent-move-p last-move)
        (remove-if-not #'(lambda (move) (= (reference move) l))
                       d-possibilities)
        d-possibilities)))

(defparameter *d-ruleset*
  (make-instance 'ruleset
                 :expander #'d-propositional-expander
                 :description "Felscher's D ruleset, for propositional languages."))

(defparameter *e-ruleset*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander
                 :description "Felscher's E ruleset, for propositional languages."))
