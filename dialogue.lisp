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

(defclass dialogue ()
  ((plays
    :accessor plays
    :type list
    :initform nil
    :initarg :plays)
   (initial-formula
    :accessor initial-formula
    :type formula
    :initarg :initial-formula)
   (concessions
    :type list
    :reader concessions
    :initform nil
    :initarg :concessions
    :documentation "A list of formulas that are initially conceded by the Opponent.  (Another term is \"common ground\".)")
   (ruleset
    :type ruleset
    :accessor ruleset
    :initarg :ruleset
    :initform (error "To make a dialogue, a ruleset is required."))))

(defun empty-dialogue-p (dialogue)
  (null (plays dialogue)))

(defun truncate-dialogue (dialogue cutoff)
  (make-instance 'dialogues::dialogue
		 :plays (subseq (plays dialogue) 0 cutoff)
                 :concessions (concessions dialogue)
                 :initial-formula (initial-formula dialogue)))

(defmethod print-object ((game dialogue) stream)
  (print-unreadable-object (game stream :type t)
    (with-slots (plays initial-formula ruleset concessions) game
      (format stream "Ruleset: ~a" ruleset)
      (terpri stream)
      (cond ((length= concessions 0)
             (format stream "No concessions."))
            ((length= concessions 1)
             (format stream "1 concession:")
             (terpri stream)
             (format stream "~a" (first concessions))
             (terpri stream))
            (t
             (format stream "~d concessions:" (length concessions))
             (terpri stream)
             (loop
                :for f :in concessions
                :do
                (format stream "~a" f)
                (terpri stream))))
      (if (null plays)
          (format stream "1 move")
          (format stream "~d moves" (1+ (length plays))))
      (format stream ":")
      (terpri stream)
      (format stream "0 P ~a [initial move]" initial-formula)
      (unless (null plays)
        (loop
           :initially (format stream "~%")
           :for move :in plays
           :for i :from 1 :upto (length plays)
           :do
           (format stream "~d " i)
           (if (proponent-move-p move)
               (format stream "P ")
               (format stream "O "))
           (format stream "~A [" (statement move))
           (if (attack-p move)
                 (format stream "A")
                 (format stream "D"))
           (format stream ",~d]" (reference move))
           (terpri stream))))))

(defun dialogue-length (dialogue)
  (1+ (length (plays dialogue))))

(defun add-move-to-dialogue (dialogue move)
  (make-instance 'dialogues::dialogue
                 :plays (append (plays dialogue) (list move))
                 :initial-formula (initial-formula dialogue)
                 :concessions (concessions dialogue)
                 :ruleset (ruleset dialogue)))

(defun nth-move (dialogue n)
  (if (zerop n)
      (error "What is the zeroth move?")
      (nth (1- n) (plays dialogue))))

(defun last-move (dialogue)
  (first (last (plays dialogue))))

(defun nth-statement (dialogue n)
  (if (zerop n)
      (initial-formula dialogue)
      (statement (nth-move dialogue n))))

(defun attacking-moves (dialogue)
  (remove-if-not #'attack-p (plays dialogue)))

(defun defensive-moves (dialogue)
  (remove-if-not #'defense-p (plays dialogue)))

(defun moves-referring-to (dialogue reference)
  (remove-if-not #'(lambda (x) (= x reference))
                 (plays dialogue)
                 :key #'reference))

(defun attacks-referring-to (dialogue reference)
  (remove-if-not #'attack-p (moves-referring-to dialogue reference)))

(defun closed-attack-indices (dialogue)
  (mapcar #'reference (defensive-moves dialogue)))

(defun open-attack-indices (dialogue)
  (unless (empty-dialogue-p dialogue)
    (loop
       :with indices = nil
       :with moves = (plays dialogue)
       :for m :in moves
       :for i :from 1
       :do
       (when (attack-p m)
         (unless (some #'(lambda (other-move)
                           (and (defense-p other-move)
                                (= (reference other-move) i)))
                       (subseq moves i))
           (push i indices)))
       :finally
       (return (reverse indices)))))

(defun most-recent-open-attack-on-opponent (dialogue)
  (loop
     :with plays = (plays dialogue)
     :for i :from (dialogue-length dialogue) :downto 1
     :for tail = (subseq plays (1- i))
     :for move = (nth-move dialogue i)
     :when (and (proponent-move-p move)
                (attack-p move)
                (not (some #'(lambda (other-move)
                               (and (opponent-move-p other-move)
                                    (defense-p other-move)
                                    (= (reference other-move) i)))
                           tail))) :do (return i)
     :finally (return nil)))

(defun most-recent-open-attack-on-proponent (dialogue)
  (loop
     :with plays = (plays dialogue)
     :for i :from (dialogue-length dialogue) :downto 1
     :for tail = (subseq plays (1- i))
     :for move = (nth-move dialogue i)
     :when (and (opponent-move-p move)
                (attack-p move)
                (not (some #'(lambda (other-move)
                               (and (proponent-move-p other-move)
                                    (defense-p other-move)
                                    (= (reference other-move) i)))
                           tail))) :do (return i)
     :finally (return nil)))

(defun most-recent-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (first open-attacks)))

(defun earliest-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (first (last open-attacks))))

(defun continuations (dialogue)
  "A list of moves according to which DIALOGUE could be continued."
  (loop
     :with ruleset = (ruleset dialogue)
     :with l = (funcall (expander ruleset) dialogue)
     :with validator = (validator ruleset)
     :for move :in l
     :for extended-dialogue = (add-move-to-dialogue dialogue move)
     :for check = (funcall validator extended-dialogue)
     :do
     (unless check
       (error "Adding the move~%~%  ~a~%~%to the dialogue~%~%~a~%~%violates the ruleset." move dialogue))
     :finally
     (return (remove-duplicates l :test #'equal-moves?))))

(defun next-attacks (dialogue)
  (remove-if-not #'attack-p (continuations dialogue)))

(defun next-proponent-attacks (dialogue)
  (remove-if-not #'proponent-move-p (next-attacks dialogue)))

(defun next-opponent-attacks (dialogue)
  (remove-if-not #'opponent-move-p (next-attacks dialogue)))

(defun next-defenses (dialogue)
  (remove-if-not #'defense-p (continuations dialogue)))

(defun next-proponent-defenses (dialogue)
  (remove-if-not #'proponent-move-p (next-defenses dialogue)))

(defun next-opponent-defenses (dialogue)
  (remove-if-not #'opponent-move-p (next-defenses dialogue)))

(defun next-proponent-moves (dialogue)
  (remove-if-not #'proponent-move-p (continuations dialogue)))

(defun next-opponent-moves (dialogue)
  (remove-if-not #'opponent-move-p (continuations dialogue)))

(defun proponent-wins-p (dialogue)
  (and (proponent-move-p (last-move dialogue))
       (null (next-opponent-moves dialogue))))

(defun opponent-wins-p (dialogue)
  (and (opponent-move-p (last-move dialogue))
       (null (next-proponent-moves dialogue))))

(defun proponent-loses-p (dialogue)
  (not (proponent-wins-p dialogue)))

(defun opponent-loses-p (dialogue)
  (not (opponent-wins-p dialogue)))

(defmethod terms-in ((dialogue dialogue))
  (terms-in (plays dialogue)))

(defmethod free-variables ((dialogue dialogue))
  (free-variables (plays dialogue)))

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

(defun e-opponent-attacks (node)
  (let ((formula (state node)))
    (cond ((atomic-formula-p formula)
           (list (make-instance 'dialogue-node
                                :action (make-instance 'move
                                                       :statement *attack-atom*
                                                       :reference formula
                                                       :attack t)
                                :parent node)))
          ((implication-p formula)
           (list (make-instance 'dialogue-node
                                :action (make-instance 'move
                                                       :statement (antecedent formula)
                                                       :reference formula
                                                       :attack t)
                                :parent node)))
          ((negation-p formula)
           (list (make-instance 'dialogue-node
                                :action (make-instance 'move
                                                       :statement (unnegate formula)
                                                       :reference formula
                                                       :attack t)
                                :parent node)))
          (t
           (error "This is an Opponent node, but we don't know how to attack~%~%  ~a~%" formula)))))

(defun e-opponent-defenses (node)
  (declare (ignore node))
  nil)

(defun e-proponent-attacks (node)
  (loop
     :with attacks = nil
     :with asserted = (opponent-assertions-by-occurrence node)
     :for formula :in asserted
     :do
     (cond ((atomic-formula-p formula)
            ;; Proponent cannot attack atoms
            )
           ((implication-p formula)
            (push (make-instance 'dialogue-node
                                 :action (make-instance 'move
                                                        :attack t
                                                        :reference formula
                                                        :statement (antecedent formula))
                                 :parent node)
                  attacks))
           ((negation-p formula)
            (push (make-instance 'dialogue-node
                                 :action (make-instance 'move
                                                        :attack t
                                                        :reference formula
                                                        :statement (unnegate formula))
                                 :parent node)
                  attacks))
           (t
            (error "Don't know how to compute Proponent attacks on~%~%  ~a~%~%" formula)))
     :finally
     (return attacks)))

(defun e-proponent-defenses (node)
  (loop
     :with defenses = nil
     :with attacked = (opponent-attacked-formulas-by-occurrence node)
     :for formula :in attacked
     :do
     (cond ((atomic-formula-p formula)
            ;; Proponent cannot defend against attacks on atoms
            )
           ((implication-p formula)
            (push (make-instance 'dialogue-node
                                 :action (make-instance 'move
                                                        :attack nil
                                                        :reference formula
                                                        :statement (consequent formula))
                                 :parent node)
                  defenses))
           ((negation-p formula)
            ;; no defense against negations
            )
           (t
            (error "Don't know how to compute Proponent defenses against~%~%  ~a~%~%" formula)))
     :finally
     (return defenses)))

(defun e-propositional-expander (node)
  (cond ((opponent-node-p node)
         (append (e-opponent-attacks node)
                 (e-opponent-defenses node)))
        ((proponent-node-p node)
         (append (e-proponent-attacks node)
                 (e-proponent-defenses node)))
        (t
         (error "The node~%~%  ~a~%~%seems to be neither a Proponent node nor an Opponent node." node))))

(defun e-propositional-validator (dialogue)
  (declare (ignore dialogue))
  t)

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

;;; dialogues.lisp ends here
