(in-package :dialogues)

(defclass ruleset ()
  ((expander
    :type function
    :accessor expander
    :initform (error "A ruleset needs an expander function.")
    :initarg :expander
    :documentation "A unary function that takes a dialogue and returns a list of moves by which the given dialogue can be continued.")
   (validator
    :type function
    :accessor validator
    :initform (error "A ruleset needs a validator function.")
    :initarg :validator
    :documentation "A unary function that takes a dialogue and returns T or NIL according as the given dialogue does or does not adhere to the ruleset.")
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
            (opponent-assertions dialogue)
            :test #'equal-formulas?)))

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
                 ((atomic-formula-p statement)
                  nil)
                 ((generalization-p statement)
                  nil)
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
                               ((generalization-p attacked-statement)
                                nil)
                               ((atomic-formula-p attacked-statement)
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

(defun d-fol-proponent-attacks (dialogue term-depth)
  (when (opponent-move-p (last-move dialogue))
    (loop
       :with responses = nil
       :with l = (dialogue-length dialogue)
       :for i :from 1 :upto (1- l) :by 2
       :for move = (nth-move dialogue i)
       :for statement = (statement move)
       :do
       (when (non-atomic-formula-p statement)
         (cond ((universal-generalization-p statement)
                (let ((terms (cons (fresh-variable dialogue)
                                   (append (non-variable-terms-in dialogue)
                                           (free-variables dialogue)))))
                  (setf terms (remove-if-not #'(lambda (d)
                                                 (<= d term-depth))
                                             terms
                                             :key #'term-depth))
                  (dolist (term terms) ;; do we need to consider a fresh variable here?
                    (push (make-instance 'proponent-move
                                         :attack t
                                         :reference i
                                         :statement (make-instance 'which-instance-attack :instance term :name (format nil "?-~a" term)))
                          responses))))
               ((existential-generalization-p statement)
                (let ((instance (fresh-variable dialogue)))
                  (push (make-instance 'proponent-move
                                       :attack t
                                       :reference i
                                       :statement (make-instance 'which-instance-attack :instance instance :name (format nil "?-~a" instance)))
                        responses)))))
       :finally
       ;; ensure that the atom restriction on Proponent is observed
       (return (remove-if #'(lambda (move)
                              (and (atomic-formula-p move)
                                   (not (opponent-asserted-atom-earlier? dialogue move))))
                          responses)))))

(defun d-fol-opponent-attacks (dialogue term-depth)
  (when (proponent-move-p (last-move dialogue))
    (loop
       :with responses = nil
       :with l = (dialogue-length dialogue)
       :for i :from 2 :upto (1- l) :by 2
       :for move = (nth-move dialogue i)
       :for statement = (statement move)
       :do
       (when (non-atomic-formula-p statement)
         (cond ((universal-generalization-p statement)
                (let ((instance (fresh-variable dialogue)))
                  (push (make-instance 'opponent-move
                                       :attack t
                                       :reference i
                                       :statement (make-instance 'which-instance-attack :instance instance :name (format nil "?-~a" instance)))
                              responses)))
               ((existential-generalization-p statement)
                (let ((terms (cons (fresh-variable dialogue)
                                   (append (non-variable-terms-in dialogue)
                                           (free-variables dialogue)))))
                  (setf terms (remove-if-not #'(lambda (d)
                                                 (<= d term-depth))
                                             terms
                                             :key #'term-depth))
                  (dolist (term terms) ;; do we need to consider a fresh variable here?
                    (push (make-instance 'opponent-move
                                         :attack t
                                         :reference i
                                         :statement (make-instance 'which-instance-attack :instance term :name (format nil "?-~a" term))) responses))))))
       :finally (return responses))))

(defun d-fol-attacks (dialogue term-depth)
  (append (d-possible-attacks dialogue)
          (d-fol-proponent-attacks dialogue term-depth)
          (d-fol-opponent-attacks dialogue term-depth)))

(defun d-fol-opponent-defenses (dialogue term-depth)
  (declare (ignore term-depth))
  (let* ((responses nil)
         (last-move (last-move dialogue))
         (most-recent (most-recent-open-attack-on-opponent dialogue)))
    (when (proponent-move-p last-move)
      (when (integerp most-recent)
        (let* ((attack (nth-move dialogue most-recent))
               (attack-reference (reference attack))
               (attacked-statement (if (zerop attack-reference)
                                       (initial-formula dialogue)
                                       (nth-statement dialogue attack-reference))))
          (let ((defenses
                 (when (which-instance-attack-p attack)
                   (let ((instance (instance (statement attack))))
                     (cond ((universal-generalization-p attacked-statement)
                            (list (instantiate attacked-statement instance (first (bindings attacked-statement)))))
                           ((existential-generalization-p attacked-statement)
                            (let ((fresh (fresh-variable dialogue)))
                              (list (instantiate attacked-statement fresh (first (bindings attacked-statement)))))))))))
            (dolist (defense defenses)
              (push (make-instance 'opponent-move
                                   :reference most-recent
                                   :statement defense
                                   :attack nil)
                    responses))))))
    ;; filter out duplicate Opponent attacks
    (remove-if #'(lambda (move)
                   (duplicate-opponent-attack? dialogue move))
               responses)))

(defun d-fol-proponent-defenses (dialogue term-depth)
  (declare (ignore term-depth))
  (let* ((responses nil)
         (last-move (last-move dialogue))
         (other-class 'proponent-move)
         (most-recent (most-recent-open-attack-on-proponent dialogue)))
    (when (opponent-move-p last-move)
      (when (integerp most-recent)
        (let* ((attack (nth-move dialogue most-recent))
               (attack-reference (reference attack))
               (attacked-statement (if (zerop attack-reference)
                                       (initial-formula dialogue)
                                       (nth-statement dialogue attack-reference))))
          (let ((defenses
                 (when (which-instance-attack-p attack)
                   (let ((instance (instance (statement attack))))
                     (cond ((universal-generalization-p attacked-statement)
                            (unless (variable-term-p instance)
                              (error "Opponent attacked a universal by asking~%~%  ~a~%~%which is not asking for a variable." attack))
                            (list (instantiate attacked-statement instance (first (bindings attacked-statement)))))
                           ((existential-generalization-p attacked-statement)
                            (list (instantiate attacked-statement instance (first (bindings attacked-statement))))))))))
            (dolist (defense defenses)
              (push (make-instance other-class
                                   :reference most-recent
                                   :statement defense
                                   :attack nil)
                    responses))))))
    ;; ensure that the atom restriction on Proponent is observed
    (remove-if #'(lambda (move)
                   (and (atomic-formula-p move)
                        (not (opponent-asserted-atom-earlier? dialogue move))))
               responses)))

(defun d-fol-defenses (dialogue term-depth)
  (append (d-possible-defenses dialogue)
          (d-fol-opponent-defenses dialogue term-depth)
          (d-fol-proponent-defenses dialogue term-depth)))

(defun d-propositional-expander (dialogue)
  ;; Rules: (1) an attack may be defended only once; (2) P may not
  ;; assert an atom before O; (3) only the most recent open attack may
  ;; be defended.
  (append (d-possible-defenses dialogue)
          (d-possible-attacks dialogue)))

(defun moves-alternate-p (dialogue)
  (loop
     :with plays = (plays dialogue)
     :for move :in plays
     :for i :from 1
     :do
     (cond ((oddp i)
            (unless (opponent-move-p move) (return nil)))
           ((evenp i)
            (unless (proponent-move-p move) (return nil))))
     :finally (return t)))

(defun every-defense-responds-to-most-recent-open-attack (dialogue)
  "Is every defense in DIALOGUE against the most recent open attack?"
  (loop
     :with plays = (plays dialogue)
     :for j :from 1
     :for move :in plays
     :do
     (when (defense-p move)
       (let ((i (reference move)))
         (let ((range (subseq plays 0 i)))
           (when (some #'(lambda (other-move)
                           (and (defense-p move)
                                (= (reference other-move) i)))
                       range)
                   (return nil)))))
     :finally (return t)))

(defun no-duplicate-defenses (dialogue)
  "Determine whether every attack in DIALOGUE is defended at most once.

This predicate is redundant when EVERY-DEFENSE-RESPONDS-TO-MOST-RECENT-OPEN-ATTACK is in force."
  (declare (ignore dialogue))
  t)

(defun proponent-assertions-attacked-at-most-once (dialogue)
  "Proponent's assertions are not attacked more than once."
  (loop
     :with plays = (plays dialogue)
     :for i :from 0
     :for move :in plays
     :do
     (when (and (opponent-move-p move) (attack-p move))
       (let ((j (reference move)))
         (when (some #'(lambda (other-move)
                         (and (opponent-move-p other-move)
                              (attack-p other-move)
                              (= (reference other-move) j)))
                     (subseq plays 0 i))
           (return nil))))
     :finally (return t)))

(defun d-propositional-validator (dialogue)
  (let ((initial (initial-formula dialogue)))
    (when (non-atomic-formula-p initial)
      (when (moves-alternate-p dialogue)
        (when (every-defense-responds-to-most-recent-open-attack dialogue)
          (when (no-duplicate-defenses dialogue)
            (proponent-assertions-attacked-at-most-once dialogue)))))))

(defun d-fol-expander (dialogue term-depth)
  ;; Rules: (1) an attack may be defended only once; (2) P may not
  ;; assert an atom before O; (3) only the most recent open attack may
  ;; be defended.  No term will be considerd having depth exceeding
  ;; TERM-DEPTH.
  (append (d-fol-defenses dialogue term-depth)
          (d-fol-attacks dialogue term-depth)))

(defun e-propositional-expander (dialogue)
  ;; Rules: E rules, with the restriction that Opponent must respond immediately to Proponent.
  (let ((last-move (last-move dialogue))
        (i (1- (dialogue-length dialogue)))
        (d-possibilities (d-propositional-expander dialogue)))
    (if (proponent-move-p last-move)
        (remove-if-not #'(lambda (move) (= (reference move) i))
                       d-possibilities)
        d-possibilities)))

(defun e-propositional-validator (dialogue)
  (and (d-propositional-validator dialogue)
       (loop
          :with plays = (plays dialogue)
          :for move :in plays
          :for i :from 0
          :do (when (opponent-move-p move)
                (unless (= (reference move) i)
                  (return nil)))
          :finally (return t))))

(defun e-fol-expander (dialogue term-depth)
  ;; Rules: E rules, with the restriction that Opponent must respond immediately to Proponent.
  (let ((last-move (last-move dialogue))
        (i (1- (dialogue-length dialogue)))
        (d-possibilities (d-fol-expander dialogue term-depth)))
    (if (proponent-move-p last-move)
        (remove-if-not #'(lambda (move) (= (reference move) i))
                       d-possibilities)
        d-possibilities)))

(defun d-fol-validator (dialogue)
  (and (d-propositional-validator dialogue)
       (loop
          :with plays = (plays dialogue)
          :for move :in plays
          :for i :from 1
          :for statement = (statement move)
          :do
          (cond ((attack-p move)
                 (let ((attack-reference (reference move)))
                   (let ((attacked-statement (nth-statement dialogue attack-reference)))
                     (when (generalization-p attacked-statement)
                       (when (which-instance-attack-p move)
                         (cond ((universal-generalization-p attacked-statement)
                                (let ((instance (instance statement)))
                                  (cond ((proponent-move-p attacked-statement)
                                         (when (variable-term-p instance)
                                           (unless (occurs-freely instance (truncate-dialogue dialogue i))
                                             (return nil))))
                                        ((opponent-move-p attacked-statement)
                                         t))))
                               ((existential-generalization-p attacked-statement)
                                (let ((instance (instance statement)))
                                  (cond ((proponent-move-p attacked-statement)
                                         t)
                                        ((opponent-move-p attacked-statement)
                                         (unless (occurs-freely instance (truncate-dialogue dialogue i))
                                           (return nil)))))))))))))
          :finally (return t))))

(defun e-fol-validator (dialogue)
  (when (d-fol-validator dialogue)
    (e-propositional-validator dialogue)))

(defun e-propositional-expander--prefer-defenses (dialogue)
  ;; Rules: E rules, with the restriction that Opponent must respond immediately to Proponent.  Additionally, if Proponent can defend, then he will.
  (let ((last-move (last-move dialogue))
        (e-possibilities (e-propositional-expander dialogue)))
    (if (opponent-move-p last-move)
        (let ((defenses (remove-if-not #'defense-p e-possibilities))
              (non-defenses (remove-if #'defense-p e-possibilities)))
          (append defenses non-defenses))
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

(defun e-fol-expander--no-repetitions (dialogue term-depth)
  (remove-if #'(lambda (move)
                 (move-is-repetition dialogue move))
             (e-fol-expander dialogue term-depth)))

(defun e-fol-expander--no-repetitions+prefer-defenses (dialogue term-depth)
  (let ((last-move (last-move dialogue))
        (e-possibilities (e-fol-expander--no-repetitions dialogue term-depth)))
    (if (opponent-move-p last-move)
        (let ((defenses (remove-if-not #'defense-p e-possibilities)))
          (if (null defenses)
              e-possibilities
              (list (first defenses))))
        e-possibilities)))

(defparameter *d-ruleset*
  (make-instance 'ruleset
                 :expander #'d-propositional-expander
                 :description "Felscher's D ruleset, for propositional languages."
                 :validator #'d-propositional-validator))

(defparameter *e-ruleset*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander
                 :description "Felscher's E ruleset, for propositional languages."
                 :validator #'e-propositional-validator))

(defparameter *e-ruleset--prefer-defenses*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander--prefer-defenses
                 :description "Felscher's E ruleset, for propositional languages, with the additional rule that if Proponent can defend, then he will."
                 :validator #'e-propositional-validator))

(defparameter *e-ruleset--no-repetitions*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander--no-repetitions
                 :description "Felscher's E ruleset, for propositional languages, with the restriction that no repetitions are permitted."
                 :validator #'e-propositional-validator))
