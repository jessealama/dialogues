;;; symbolic-attack.lisp Symbolic attacks

(in-package :dialogues)

(defclass symbolic-attack ()
  ((name
    :initarg :name
    :accessor name
    :initform nil)))

;; Singleton pattern: given a name, there should be one and only one
;; instance of SYMBOLIC-ATTACK with that name
(let ((symbolic-attack-table (make-hash-table :test #'equal)))
  (defmethod make-instance :around ((class (eql 'symbolic-attack)) 
				    &rest initargs)
    (let ((name-tail (member :name initargs)))
      (if name-tail
	  (let ((name (second name-tail)))
	    (if (string= name "")
		(error "To make a symbolic attack object, you must supply a name; the empty string is not an acceptable name")
		(multiple-value-bind (instance existing?)
		    (gethash name symbolic-attack-table)
		  (if existing?
		      instance
		      (let ((new-sa (call-next-method)))
			(setf (gethash name symbolic-attack-table)
			      new-sa))))))
	  (error "To make a symbolic attack object, you must supply a name"))))
  (defun symbolic-attack-table ()
    symbolic-attack-table))

(defun symbolic-attack? (obj)
  (member obj *propositional-symbolic-attacks*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing symbolic attacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((sa symbolic-attack) stream)
  (with-slots (name)
      sa
    (if name
	(format stream "~:(~a~)" name)
	(format stream "(nameless symbolic attack"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard symbolic attacks for propositional and first-order languges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *attack-left-conjunct* (make-instance 'symbolic-attack
		   :name "attack-left-conjunct"))

(defparameter *attack-right-conjunct* (make-instance 'symbolic-attack
		   :name "attack-right-conjunct"))

(defparameter *which-disjunct?* (make-instance 'symbolic-attack
		   :name "which-disjunct?"))

(defparameter *which-instance?* (make-instance 'symbolic-attack
		   :name "which-instance?"))

(defparameter *propositional-symbolic-attacks*
  (list *attack-left-conjunct*
	*attack-right-conjunct*
	*which-instance?*)
  "The three symbolic attacks that are permitted in propositional
  dialogue games, comprising the moves:

- attack the left conjunct,
- attack the right conjunct, and
- attack a disjunction by requesting one of the disjuncts.")

(defmethod render-plainly ((sa (eql *attack-left-conjunct*)))
  "&and;(L)")

(defmethod render-plainly ((sa (eql *attack-right-conjunct*)))
  "&and;(R)")

(defmethod render-plainly ((sa (eql *which-instance?*)))
  "?")

(defmethod render-plainly ((sa (eql *which-disjunct?*)))
  "?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UCW functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render ((sa (eql *attack-left-conjunct*)))
  (<:as-is "&or;")
  (<:sub "L"))

(defmethod render ((sa (eql *attack-right-conjunct*)))
  (<:as-is "&and;")
  (<:sub "R"))

(defmethod render ((sa (eql *which-instance?*)))
  (<:as-is "?"))

(defmethod render ((sa (eql *which-disjunct?*)))
  (<:as-is "?"))

;;; symbolic-attack ends here