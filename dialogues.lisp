;;; dialogues.lisp Play Lorenzen dialogue games

(defstruct move
  statement
  attack-or-defense
  reference)

(defun print-move-at-position (position move stream)
  (let ((statement (move-statement move))
	(stance (move-attack-or-defense move))
	(ref-index (move-reference move)))
    (if (evenp position)
	(if (zerop position)
	    (format stream "0 P ~A" statement)
	    (format stream "~A P [~A,~A] ~A" position
		                             stance
					     ref-index
					     statement))
	(format stream "~A O [~A,~A] ~A" position 
	                                 stance
					 ref-index
					 statement))))

(defun print-dialogue (dialogue stream depth)
  (declare (ignore depth))
  (let ((plays (dialogue-plays dialogue)))
    (when plays
      (do ((i 0 (1+ i))
	   (moves (cdr plays) (cdr moves))
	   (move (car plays) (car moves)))
	  ((null moves) (print-move-at-position i move stream))
	(print-move-at-position i move stream)
	(format stream "~%")))))


(defstruct (dialogue
	     (:print-function print-dialogue))
  (plays nil :type list))

(defun next-moves (dialogue)
  "The set of moves by which DIALOGUE can be legally extended."
  (declare (ignore dialogue))
  nil)

(defun proof-to-strategy (d)
  "Transform the deduction D with endformula x into a strategy for
  winning a dialogue game based on x."
  (declare (ignore d))
  nil)

(defun play-as-proponent (formula)
  "Play a dialogue game for FORMULA as proponent."
  (declare (ignore formula))
  nil)

(defun play-as-opponent (formula)
  "Play a dialogue game for FORMULA as opponent."
  (declare (ignore formula))
  nil)



(provide 'dialogues)

;;; dialogues.lisp ends here