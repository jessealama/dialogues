;;; dialogue-search.lisp Systematically explore dialogue games as search trees

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'dialogues "dialogues.lisp"))

(defun dialogue-successors (dialogue rules)
  (let (extensions)
    (dolist (player '(p o) extensions)
      (dolist (stance '(a d))
	(let ((next-moves (next-moves dialogue rules player stance)))
	  (setf extensions
		(append extensions
			(mapcar #'(lambda (next-move)
				    (destructuring-bind (statement index)
					next-move
				      (freshly-extend-dialogue dialogue
							       player
							       stance
							       statement
							       index)))
				next-moves))))))))
(defvar *dialogue* nil)
(defvar *rules* nil)
(defvar *remainder* nil)
(defvar *all-done?* nil)

(defun dialogue-bfs-toplevel (dialogue rules)
  (cond (*remainder*
	 (error "The variable *REMAINDER* has a non-null value; don't you want to save the old results?"))
	(*rules* 
	 (error "The variable *RULES* has a non-null value -- why?"))
	(*dialogue*
	 (error "The variable *DIALOGUE* has a non-null value -- why?"))
	(t
	 (setf *all-done?* nil)
	 (setf *dialogue* dialogue)
	 (setf *rules* rules)
	 (dialogue-bfs dialogue rules nil))))

(defun d-dialogue-bfs-toplevel (dialogue)
  (dialogue-bfs-toplevel dialogue d-dialogue-rules))

(defun e-dialogue-bfs-toplevel (dialogue)
  (dialogue-bfs-toplevel dialogue d-dialogue-rules))

(defun clear-bfs-state ()
  (setf *dialogue* nil
	*rules* nil
	*all-done?* nil
	*remainder* nil))

(defun fresh-dialogue-bfs (dialogue rules)
  (clear-bfs-state)
  (dialogue-bfs dialogue rules))

(defun fresh-d-dialogue-bfs (dialogue)
  (fresh-dialogue-bfs dialogue d-dialogue-rules))

(defun fresh-e-dialogue-bfs (dialogue)
  (fresh-dialogue-bfs dialogue e-dialogue-rules))

(defun dialogue-bfs (dialogue rules &optional (yet-to-consider nil))
  (unless *all-done?*
    (let (current-dialogue solution)
      (unless yet-to-consider
	(push dialogue yet-to-consider))
      (until (or solution (null yet-to-consider))
	(setf current-dialogue (pop yet-to-consider))
	(let ((successors (dialogue-successors current-dialogue rules)))
	  (if successors
	      (setf yet-to-consider
		    (append yet-to-consider successors))
	      (setf solution current-dialogue))))
      (setf *remainder* yet-to-consider)
      (unless *remainder*
	(setf *all-done?* t))
      solution)))

(defun continue-dialogue-bfs ()
  (dialogue-bfs *dialogue* *rules* *remainder*))

(provide 'dialogue-search)

;;; dialogue-search.lisp ends here