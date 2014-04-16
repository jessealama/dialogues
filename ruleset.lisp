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

(defun e-propositional-expander (dialogue)
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
           ;; Generate all possible defenses.  Rules: (1) an
           ;; attack may be defended only once; (2) P may not assert
           ;; an atom before O; (3) only the most recent open attack
           ;; may be defended
           (let ((most-recent (most-recent-open-attack dialogue)))
             (when (integerp most-recent)
               (let* ((attack (nth-move dialogue most-recent))
                      (attack-reference (reference attack))
                      (attack-statement (statement attack))
                      (attacked-statement (nth-statement dialogue attack-reference))
                      (defense (defend-against attacked-statement attack-statement)))
                 (push (make-instance (if (opponent-move-p last-move)
                                          'proponent-move
                                          'opponent-move)
                                      :reference most-recent
                                      :statement defense
                                      :attack nil)
                       responses))))
           ;; all possible attacks
           ;; filter out illegal assertions by P of atoms
           (when (opponent-move-p last-move)
             (setf responses
                   (remove-if-not #'opponent-asserted-atom-earlier?
                                  responses)))))
    responses))

(defparameter *e-ruleset*
  (make-instance 'ruleset
                 :expander #'e-propositional-expander
                 :description "The E ruleset, for propositional languages."))
