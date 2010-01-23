;;; dialogues.lisp Play Lorenzen dialogue games

(require 'utils "utils.lisp")
(require 'formulas "formulas.lisp")

(defstruct move
  statement
  stance
  reference)

(defun attacking-move? (move)
  (eq (move-stance move) 'A))

(defun defensive-move? (move)
  (eq (move-stance move) 'D))

(defun print-move-at-position (position move stream)
  (let ((statement (move-statement move))
	(stance (move-stance move))
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

(defun make-dialogue (initial-statement)
  (let ((first-move (make-move :statement initial-statement
			       :stance nil
			       :reference nil)))
    (make-dialogue-int :initial-statement initial-statement
		       :plays (list first-move))))

(defstruct (dialogue
	     (:print-function print-dialogue)
	     (:constructor make-dialogue-int))
  initial-statement
  plays)

(defun dialogue? (d)
  "Determine whether the dialogue data structure D really does represent a Lorenzen dialogue."
  (unless (atomic-formula? (dialogue-initial-statement d))
    (let ((plays (dialogue-plays d)))
      (let ((len (length plays)))
	(let ((plays-vec (list-to-array plays)))
	  (or (zerop len)
	      (do ((i 0 (1+ i))
		   (ok? t)
		   (move (aref plays-vec 0) (aref plays-vec i)))
		  ((and ok? (= i len)) ok?)
		(unless (= i 0)
		  (let ((ref (move-reference move)))
		    (unless (and (>= ref 0)
				 (< ref i)
				 (not (= (mod ref 2) (mod i 2)))
				 (or (attacking-move? move)
				     (let ((old-move (aref plays-vec ref)))
				       (attacking-move? old-move))))
		      (setq ok? nil)))))))))))

(defun next-moves (dialogue)
  "The set of moves by which DIALOGUE can be legally extended."
  (declare (ignore dialogue))
  nil)

(defun proponent-wins? (dialogue)
  (let ((len (length (dialogue-plays dialogue))))
    (when (evenp len)
      (null (next-moves dialogue)))))

(defun start-dialogue (initial-statement)
  (when (atomic-formula? initial-statement)
    (error "A dialogue cannot commence with a composite formula!"))
  (let ((response nil)
	(stance nil)
	(index nil)
	(acceptable-input? nil)
	(turn-number 1)
	(dialogue (make-dialogue initial-statement)))
    (format t "Let's play a dialogue for ~A.~%" initial-statement)
    (format t "Turn number 0: Proponent asserts ~A~%" initial-statement)
    (until (eq response 'done)
      (format t "Turn number ~A.~%" turn-number)
      (format t "Type DONE to end the game now.~%")
      (setq acceptable-input? nil)
      (if (evenp turn-number)
	  (format t "Proponent's turn.~%")
	  (format t "Opponent's turn.~%"))
      (until acceptable-input?
        (format t "Attack (A) or defend (D)? ")
	(setq stance (read-symbol 'a 'd))
	(if (eq stance 'a)
	    (format t "Attack which statement? (Your response should be a number between 0 and ~A.) " (1- turn-number))
	    (format t "Defend against which attack? (Your response should be a number between 0 and ~A. " (1- turn-number)))
	(setq index (read-non-negative-number-at-most turn-number))
	(cond ((= (mod turn-number 2) (mod index 2))
	       (format t "One cannot attack oneself or defend against one's own attacks!~%"))
	      (t (setq acceptable-input? t)
	         (incf turn-number)))))
    (format t "Thanks for playing.~%")
    (if (dialogue? dialogue)
	(if (proponent-wins? dialogue)
	    (format t "Proponent wins!")
	    (format t "Opponent wins!"))
	(format t "Oops, something went wrong -- that sequence of moves isn't a dialogue!"))))

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