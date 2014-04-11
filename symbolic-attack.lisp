;;; symbolic-attack.lisp Symbolic attacks

(in-package :dialogues)

(defclass symbolic-attack ()
  ((name
    :initarg :name
    :type string
    :accessor name
    :initform (error "A symbol attack needs a name."))))

(defmethod print-object ((sa symbolic-attack) stream)
  (format stream "~:(~a~)" (name sa)))

(defclass which-instance-attack (symbolic-attack)
  ((instance
    :initarg :instance
    :accessor instance
    :type (or null term)
    :initform nil)))

(defmethod print-object ((sa which-instance-attack) stream)
  (let ((instance (instance sa)))
    (if (null instance)
        (format stream "?")
        (format stream "?-~a" instance))))

(defun which-instance-attack-p (x)
  (eql (class-of x) 'which-instance-attack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard symbolic attacks for propositional and first-order languges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *attack-left-conjunct*
  (make-instance 'symbolic-attack
                 :name "attack-left-conjunct"))

(defparameter *attack-right-conjunct*
  (make-instance 'symbolic-attack
                 :name "attack-right-conjunct"))

(defparameter *which-disjunct?*
  (make-instance 'symbolic-attack
                 :name "which-disjunct?"))

(defparameter *propositional-symbolic-attacks*
  (list *attack-left-conjunct*
	*attack-right-conjunct*
	*which-disjunct?*)
  "The three symbolic attacks that are permitted in propositional
  dialogue games:

- attack the left conjunct,
- attack the right conjunct, and
- attack a disjunction by requesting one of the disjuncts.")

(defun symbolic-attack-p (obj)
  (typep obj 'symbolic-attack))

;;; symbolic-attack ends here
