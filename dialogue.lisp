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
             (format stream "~a" (first concessions)))
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

(defun last-player (dialogue)
  (player (last-move dialogue)))

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

(defun earliest-open-attack-for-player (dialogue player &key end)
  "The smallest index (starting from 0) of the attacking move by
  PLAYER in DIALOGUE to which there is no response."
  (loop
     with plays = (plays dialogue)
     for i from 1 upto (if end end (length plays))
     for move in (cdr plays)
     do
       (when (attack-p move)
	 (let ((move-player (player move)))
	   (when (eql player move-player)
	     (unless (some #'(lambda (other-move)
			       (and (not (eql (player other-move) player))
				    (defense-p other-move)
				    (= (reference other-move) i)))
			   plays)
	       (return i)))))
     finally
       (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluating rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun evaluate-structural-rule (rule dialogue &key from-end)
;;   (funcall (predicate rule) dialogue :final-move-only from-end))

;; (defun evaluate-particle-rule (rule dialogue)
;;   (let ((precondition (precondition rule))
;; 	(body (body rule)))
;;     (loop
;;        for move in (dialogue-plays dialogue)
;;        for turn-number from 0
;;        do
;; 	 (when (funcall precondition dialogue
;; 			(move-player move)
;; 			turn-number
;; 			(move-statement move)
;; 			(move-stance move)
;; 			(move-reference move))
;; 	     (unless (funcall body dialogue
;; 			      (move-player move)
;; 			      turn-number
;; 			      (move-statement move)
;; 			      (move-stance move)
;; 			      (move-reference move))
;; 	       (return nil)))
;;        finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions of dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun next-moves-at-position (dialogue player stance position)
;;   (let ((result nil)
;; 	(subformulas (proper-subformula-occurrences (initial-formula dialogue)))
;; 	(game-len (dialogue-length dialogue)))
;;     (if (> position game-len)
;; 	nil
;; 	(dotimes (index position result)
;; 	  (dolist (statement (append subformulas
;; 				     *propositional-symbolic-attacks*))
;; 	    (let* ((provisional-move (make-move player statement stance index))
;; 		   (provisional-extension (provisionally-extend-dialogue dialogue provisional-move)))
;; 	      (when (fast-eval-entire-dialogue provisional-extension)
;;                 (let ((pair (list statement index)))
;;                   (pushnew pair result :test #'(lambda (pair-1 pair-2)
;;                                                  (destructuring-bind (statement-1 index-1)
;;                                                      pair-1
;;                                                    (destructuring-bind (statement-2 index-2)
;;                                                        pair-2
;;                                                      (and (= index-1 index-2)
;;                                                           (equal-statements? statement-1 statement-2))))))))))))))

;; (defun all-next-moves-at-position (dialogue position)
;;   (unless (zerop position) ;; not allowed to change the initial move
;;     (let ((diminished-dialogue (copy-and-truncate-dialogue dialogue position)))
;;       (append (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'p statement 'a reference)))
;; 		      (next-moves-at-position diminished-dialogue 'p 'a position))
;; 	      (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'p statement 'd reference)))
;; 		      (next-moves-at-position diminished-dialogue 'p 'd position))
;; 	      (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'o statement 'a reference)))
;; 		      (next-moves-at-position diminished-dialogue 'o 'a position))
;; 	      (mapcar #'(lambda (statement-and-reference)
;; 			  (destructuring-bind (statement reference)
;; 			      statement-and-reference
;; 			    (make-move 'o statement 'd reference)))
;; 		      (next-moves-at-position diminished-dialogue 'o 'd position))))))

;; (defun all-next-proponent-moves-at-position (dialogue position)
;;   (remove-if-not #'proponent-move?
;; 		 (all-next-moves-at-position dialogue position)))

;; (defun all-next-opponent-moves-at-position (dialogue position)
;;   (remove-if-not #'opponent-move?
;; 		 (all-next-moves-at-position dialogue position)))

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

(defun opponent-assertions (dialogue)
  (mapcar #'statement (remove-if-not #'opponent-move-p (plays dialogue))))

(defmethod terms-in ((dialogue dialogue))
  (terms-in (plays dialogue)))

(defmethod free-variables ((dialogue dialogue))
  (free-variables (plays dialogue)))

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

;;; dialogues.lisp ends here
