;;; figure.lisp An impressionistic name for deductions (natural, sequent)

(in-package :dialogues)

(defstruct figure
  content
  label
  parents)

(provide 'figure)

;;; figure.lisp ends here