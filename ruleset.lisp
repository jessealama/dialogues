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

(defun d-possible-attacks (dialogue)
  (if (empty-dialogue-p dialogue)
      (mapcar #'(lambda (attack)
                  (make-instance 'opponent-move
                                 :statement attack
                                 :reference 0
                                 :attack t))
              (possible-propositional-attacks (initial-formula dialogue)))
      (let* ((responses nil)
             (last-move (last-move dialogue))
             (other-class (if (opponent-move-p last-move)
                              'proponent-move
                              'opponent-move))
             (start (if (opponent-move-p last-move) 1 2)))
        (loop
           :with l = (dialogue-length dialogue)
           :for i :from start :upto (1- l) :by 2
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
                 ((negation-p statement)
                  (push (make-instance other-class
                                       :attack t
                                       :reference i
                                       :statement (unnegate statement))
                        responses))
                 ((binary-disjunction-p statement)
                  (push (make-instance other-class
                                       :attack t
                                       :reference i
                                       :statement *which-disjunct?*)
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
                  (error "How to attack~%~%  ~a~%~%?~%" statement))))
        ;; ensure that the atom restriction on Proponent is observed
        (when (opponent-move-p last-move)
          (setf responses
                (remove-if #'(lambda (move)
                               (and (atomic-formula-p move)
                                    (not (opponent-asserted-atom-earlier? dialogue move))))
                           responses)))
        responses)))

(defun d-possible-defenses (dialogue)
  (unless (empty-dialogue-p dialogue)
    (let* ((responses nil)
           (last-move (last-move dialogue))
           (other-class (if (opponent-move-p last-move)
                            'proponent-move
                            'opponent-move))
           (most-recent (if (opponent-move-p last-move)
                            (most-recent-open-attack-on-proponent dialogue)
                            (most-recent-open-attack-on-opponent dialogue))))
      (when (integerp most-recent)
        (let* ((attack (nth-move dialogue most-recent))
               (attack-reference (reference attack))
               (attack-statement (statement attack))
               (attacked-statement (if (zerop attack-reference)
                                       (initial-formula dialogue)
                                       (nth-statement dialogue attack-reference))))
          (let ((defenses (cond ((implication-p attacked-statement)
                                 (list (consequent attacked-statement)))
                               ((binary-disjunction-p attacked-statement)
                                (list (lhs attacked-statement)
                                      (rhs attacked-statement)))
                               ((negation-p attacked-statement)
                                nil)
                               ((binary-conjunction-p attacked-statement)
                                (cond ((eql attack-statement *attack-left-conjunct*)
                                       (list (lhs attacked-statement)))
                                      ((eql attack-statement *attack-right-conjunct*)
                                       (list (rhs attacked-statement)))
                                      (t
                                       (error "The statement~%~%  ~a~%~%was attacked by~%~%  ~a~%~%which is neither ~a nor ~a as we expect." attacked-statement attack-statement *attack-left-conjunct* *attack-right-conjunct*))))
                               (t
                                (error "How to defend~%~%  ~a~%~%?~%" attacked-statement)))))
            (dolist (defense defenses)
              (push (make-instance other-class
                                   :reference most-recent
                                   :statement defense
                                   :attack nil)
                    responses)))))
      ;; filter out duplicate Opponent attacks
      (when (proponent-move-p last-move)
        (setf responses
              (remove-if #'(lambda (move)
                             (duplicate-opponent-attack? dialogue move))
                         responses)))
      ;; ensure that the atom restriction on Proponent is observed
      (when (opponent-move-p last-move)
        (setf responses
              (remove-if #'(lambda (move)
                             (and (atomic-formula-p move)
                                  (not (opponent-asserted-atom-earlier? dialogue move))))
                         responses)))
      responses)))

(defun d-propositional-expander (dialogue)
  ;; Rules: (1) an attack may be defended only once; (2) P may not
  ;; assert an atom before O; (3) only the most recent open attack may
  ;; be defended.
  (append (d-possible-defenses dialogue)
          (d-possible-attacks dialogue)))

(defun e-propositional-expander (dialogue)
  ;; Rules: E rules, with the restriction that Opponent must respond immediately to Proponent.
  (let ((last-move (last-move dialogue))
        (i (1- (dialogue-length dialogue)))
        (d-possibilities (d-propositional-expander dialogue)))
    (if (proponent-move-p last-move)
        (remove-if-not #'(lambda (move) (= (reference move) i))
                       d-possibilities)
        d-possibilities)))

(defun e-propositional-expander--prefer-defenses (dialogue)
  ;; Rules: E rules, with the restriction that Opponent must respond immediately to Proponent.  Additionally, if Proponent can defend, then he will.  (If multiple defenses are available, one is chosen arbitrarily.)
  (let ((last-move (last-move dialogue))
        (e-possibilities (e-propositional-expander dialogue)))
    (if (opponent-move-p last-move)
        (let ((defenses (remove-if-not #'defense-p e-possibilities)))
          (if (null defenses)
              e-possibilities
              (first defenses)))
        e-possibilities)))

(defgeneric equal-moves? (move-1 move-2)
  (:documentation "Are MOVE-1 and MOVE-2 the same?"))

(defmethod equal-moves? ((move-1 proponent-move) (move-2 opponent-move))
  nil)

(defmethod equal-moves? ((move-1 opponent-move) (move-2 proponent-move))
  nil)

(defmethod equal-moves? ((move-1 proponent-move) (move-2 proponent-move))
  (when (= (reference move-1) (reference move-2))
    (cond ((and (attack-p move-1) (attack-p move-2))
           (equal-statements? (statement move-1) (statement move-2)))
          ((and (defense-p move-1) (defense-p move-2))
           (equal-statements? (statement move-1) (statement move-2)))
          (t
           nil))))

(defmethod equal-moves? ((move-1 opponent-move) (move-2 opponent-move))
  (when (= (reference move-1) (reference move-2))
    (cond ((and (attack-p move-1) (attack-p move-2))
           (equal-statements? (statement move-1) (statement move-2)))
          ((and (defense-p move-1) (defense-p move-2))
           (equal-statements? (statement move-1) (statement move-2)))
          (t
           nil))))

(defun move-is-repetition (dialogue move)
  "Has MOVE already been made in DIALOGUE?"
  (member move (plays dialogue) :test #'equal-moves?))

(defun e-propositional-expander--no-repetitions (dialogue)
  (remove-if #'(lambda (move)
                 (move-is-repetition dialogue move))
             (e-propositional-expander dialogue)))

(defparameter *d-ruleset*
  (make-instance 'ruleset
                 :expander #'d-propositional-expander
                 :description "Felscher's D ruleset, for propositional languages."))

(defparameter *e-ruleset*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander
                 :description "Felscher's E ruleset, for propositional languages."))

(defparameter *e-ruleset--prefer-defenses*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander--prefer-defenses
                 :description "Felscher's E ruleset, for propositional languages, with the additional rule that if Proponent can defend, then he will."))
