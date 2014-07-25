(in-package :dialogues)

(defclass move ()
  ((statement
    :initarg :statement
    :type (or formula symbolic-attack)
    :initform (error "Every move must contain a statement.")
    :accessor statement)
   (reference
    :initarg :reference
    :type (or formula symbolic-attack)
    :initform (error "Every move must refer to another.")
    :accessor reference)
   (attack
    :initarg :attack
    :type boolean
    :initform nil
    :accessor attack-p)))

(defmethod print-object ((m move) stream)
  (print-unreadable-object (m stream :type t :identity nil)
    (with-slots (statement reference) m
      (if (attack-p m)
          (format stream "attack")
          (format stream "defend against the attack on"))
      (format stream " ~a by asserting ~a" reference statement))))

(defun move-p (x)
  (typep x 'dialogues::move))

(defmethod atomic-formula-p ((x move))
  (atomic-formula-p (statement x)))

(defgeneric defense-p (x)
  (:documentation "Is X a defense?"))

(defmethod defense-p ((x t))
  nil)

(defmethod defense-p ((x move))
  (not (attack-p x)))

(defclass proponent-move (move)
  nil)

(defun proponent-move-p (x)
  (typep x 'dialogues::proponent-move))

(defclass opponent-move (move)
  nil)

(defun opponent-move-p (x)
  (typep x 'dialogues::opponent-move))

(defgeneric player (x)
  (:documentation "The player of X."))

(defmethod player ((x t))
  (error "What is the player of~%~%  ~a~%~%?~%" x))

(defmethod player ((x proponent-move))
  "P")

(defmethod player ((x opponent-move))
  "O")

(defmethod terms-in ((sa symbolic-attack))
  nil)

(defmethod terms-in ((m move))
  (terms-in (statement m)))

(defmethod free-variables ((m move))
  (free-variables (statement m)))

(defmethod generalization-p ((x move))
  (generalization-p (statement x)))

(defmethod universal-generalization-p ((x move))
  (universal-generalization-p (statement x)))

(defmethod existential-generalization-p ((x move))
  (existential-generalization-p (statement x)))

(defmethod which-instance-attack-p ((m move))
  (which-instance-attack-p (statement m)))
