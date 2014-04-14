(defclass move ()
  ((player :initarg :player
	   :accessor move-player)
   (statement :initarg :statement
	      :accessor move-statement)
   (stance :initarg :stance
	   :accessor move-stance)
   (reference :initarg :reference
	      :accessor move-reference)))

(defun make-move (player statement stance reference)
  (make-instance 'move
		 :player player
		 :statement statement
		 :stance stance
		 :reference reference))

(defun make-proponent-move (statement stance reference)
  (make-move 'p statement stance reference))

(defun make-opponent-move (statement stance reference)
  (make-move 'o statement stance reference))

(defun make-attack (player statement reference)
  (make-move player statement 'a reference))

(defun make-defense (player statement reference)
  (make-move player statement 'd reference))

(defun make-proponent-attack (statement reference)
  (make-attack 'p statement reference))

(defun make-opponent-attack (statement reference)
  (make-attack 'o statement reference))

(defun make-proponent-defense (statement reference)
  (make-defense 'p statement reference))

(defun make-opponent-defense (statement reference)
  (make-defense 'o statement reference))

(defun proponent-move? (move)
  (let ((player (move-player move)))
    (string= player "P")))

(defun opponent-move? (move)
  (let ((player (move-player move)))
    (string= player "O")))

(defun other-player (player)
  (case player
      (p 'o)
      (o 'p)))

(defmethod print-object ((move move) stream)
  (print-unreadable-object (move stream :type t)
    (with-slots (player statement stance reference) move
      (format stream "player: ~A stance: ~A statement: ~A (in reference to move ~A)"
	      (or player "(unset)")
	      (or stance "(unset)")
	      (or statement "(unset)")
	      (or reference "(unset)")))))

(defun pretty-print-move (move stream)
  (with-slots (stance reference statement)
      move
    (if (and stance reference) ; a non-initial move
	(format stream "[~A,~A] ~A" stance reference statement)
	(format stream "~A (initial move)" statement))))

(defun equal-moves? (move-1 move-2)
  (and (string= (symbol-name (move-player move-1)) (symbol-name (move-player move-2)))
       (string= (symbol-name (move-stance move-1)) (symbol-name (move-stance move-2)))
       (let ((ref-1 (move-reference move-1))
	     (ref-2 (move-reference move-2)))
	 (if (integerp ref-1)
	     (and (integerp ref-2) (= ref-1 ref-2))
	     (and (null ref-1) (null ref-2))))
       (equal-statements? (move-statement move-1)
			  (move-statement move-2))))

(defun attacking-move? (move)
  (string= (symbol-name (move-stance move)) "A"))

(defun defensive-move? (move)
  (string= (symbol-name (move-stance move)) "D"))

(defun initial-move? (move)
  (and (null (move-stance move))
       (null (move-reference move))))
