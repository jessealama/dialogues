(in-package :dialogues)

(defclass move ()
  ((statement :initarg :statement
              :type (or formula symbolic-attack)
              :initform (error "Every move must contain a statement.")
	      :accessor statement)
   (reference :initarg :reference
              :type integer
              :initform (error "Every move must refer to another.")
	      :accessor reference)
   (attack
    :initarg :attack
    :type boolean
    :initform nil
    :accessor attack-p)))

(defun move-p (x)
  (typep x 'move))

(defgeneric defense-p (x)
  (:documentation "Is X a defense?"))

(defmethod defense-p ((x t))
  nil)

(defmethod defense-p ((x move))
  (not (attack-p x)))

(defclass proponent-move (move)
  nil)

(defun proponent-move-p (x)
  (typep x 'proponent-move))

(defclass opponent-move (move)
  nil)

(defun opponent-move-p (x)
  (typep x 'opponent-move))

(defun initial-move-p (move)
  (not (slot-boundp move 'reference)))

(defgeneric player (x)
  (:documentation "The player of X."))

(defmethod player ((x t))
  (error "What is the player of~%~%  ~a~%~%?~%" x))

(defmethod player ((x proponent-move))
  "P")

(defmethod player ((x opponent-move))
  "O")

(defun move-< (move-1 move-2)
  (with-slots ((statement-1 statement) (stance-1 stance) (reference-1 reference))
      move-1
    (with-slots ((statement-2 statement) (stance-2 stance) (reference-2 reference))
	move-2
      (or (< reference-1 reference-2)
	  (and (= reference-1 reference-2)
	       (if (eq stance-1 'a)
		   (and (eq stance-2 'a)
			(statement-< statement-1 statement-2))
		   (and (eq stance-2 'd)
			(statement-< statement-1 statement-2))))))))
