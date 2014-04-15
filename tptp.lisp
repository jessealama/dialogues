
(in-package :dialogues)

(defclass general-list ()
  ((terms
    :type list
    :accessor terms
    :initarg :terms
    :initform nil)))

(defmethod print-object ((l general-list) stream)
  (format stream "[~{~a~^,~}]" (terms l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-formula ()
  ((name
    :initarg :name
    :initform (error "An fof needs a name.")
    :accessor name)
   (role
    :initarg :role
    :accessor role
    :initform (error "An fof needs a role."))
   (formula
    :initarg :formula
    :accessor formula
    :initform (error "An fof needs a formula."))
   (source
    :initarg :source
    :accessor source)
   (optional-info
    :initarg :optional-info
    :accessor optional-info)))

(defmethod initialize-instance :after ((x tptp-formula) &rest initargs &key &allow-other-keys)
  "Ensure that if X's optional-info slot is set, then its source slot is also set."
  (declare (ignore initargs))
  (when (slot-boundp x 'optional-info)
    (unless (slot-boundp x 'source)
      (error "A TPTP formula whose optional-info slot is bound must also have its source slot bound."))))

(defclass fof (tptp-formula)
  nil)

(defclass cnf (tptp-formula)
  nil)

(defclass internal-source (tptp-source)
  nil)

(defclass external-source (tptp-source)
  nil)

(defmethod print-object ((x tptp-formula) stream)
  (with-slots (name role formula)
      x
    (if (slot-boundp x 'source)
	(let ((source (source x)))
	  (if (slot-boundp x 'optional-info)
	      (let ((optional-info (optional-info x)))
		(format stream "(~a, ~a, ~a, ~a, ~a)." name role formula source optional-info))
	      (format stream "(~a, ~a, ~a, ~a)." name role formula source)))
	(format stream "(~a, ~a, ~a)." name role formula))))

(defmethod print-object ((x fof) stream)
  (format stream "fof")
  (call-next-method))

(defmethod print-object ((x cnf) stream)
  (format stream "cnf")
  (call-next-method))

(defgeneric render (tptp-thing)
  (:documentation "A plain text rendering of TPTP-THING."))

(defmethod render ((formula fof))
  (format nil "fof(~a,~a,~a)."
	  (name formula)
	  (role formula)
	  (formula formula)))

(defmethod render ((formula cnf))
  (format nil "cnf(~a,~a,~a)."
	  (name formula)
	  (role formula)
	  (formula formula)))

(defun sort-formula-list (formula-list)
  (let ((sorted (sort formula-list #'string< :key #'name)))
    sorted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TPTP databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-db ()
  ((formulas :type list
	     :initarg :formulas
	     :accessor formulas
	     :initform nil)
   (path
    :type (or null pathname)
    :accessor path
    :initform nil
    :initarg :path)))

(defmethod print-object ((problem tptp-db) stream)
  (format stream "~{~a~^~%~}" (formulas problem)))

(defun problem-directory (tptp-db)
  (with-slots (path)
      tptp-db
    (when (pathnamep path)
      (directory-namestring path))))

(defmethod render ((formulas list))
  (if formulas
      (format nil "~{~a~%~}" (mapcar #'render formulas))
      (format nil "(empty formula list)")))

(defmethod render ((problem tptp-db))
  (render (formulas problem)))

(defgeneric proper-formulas (problem))

(defmethod proper-formulas ((problem tptp-db))
  (mapcar #'formula (formulas problem)))

(defgeneric conjecture-formula (thing)
  (:documentation "The conjecture formula in THING."))

(defmethod conjecture-formula ((db tptp-db))
  (find "conjecture" (formulas db) :test #'string= :key #'role))

(defmethod conjecture-formula ((x null))
  nil)

(defmethod conjecture-formula ((l list))
  (find "conjecture" l :test #'string= :key #'role))

(defun has-conjecture-p (problem)
  (not (null (conjecture-formula problem))))

(defun formulas-with-status (problem status)
  (remove-if-not #'(lambda (stat) (string= stat status))
		 (formulas problem)
		 :key #'role))

(defun statuses-of-formulas (problem)
  (loop
     with statuses = (make-hash-table :test #'equal)
     for formula in (formulas problem)
     for status = (role formula)
     do (setf (gethash status statuses) 0)
     finally (return (hash-table-keys statuses))))

(defun non-conjecture-formulas (problem)
  (remove-if #'(lambda (stat) (string= stat "conjecture"))
	     (formulas problem)
	     :key #'role))

(defun formula-names (tptp-db)
  (mapcar #'name (formulas tptp-db)))

(defgeneric formula-with-name (tptp-db name))

(defmethod formula-with-name ((tptp-db tptp-db) (name integer))
  (formula-with-name tptp-db (format nil "~d" name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name symbol))
  (formula-with-name tptp-db (symbol-name name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name string))
  (find name (formulas-w/o-includes tptp-db)
	:test #'string=
	:key #'(lambda (x) (stringify (name x)))))

(defgeneric premises (problem))

(defmethod premises ((db tptp-db))
  (non-conjecture-formulas db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass include-instruction ()
  ((file
    :accessor file
    :initarg :file
    :initform (error "An include instruction requires a file name."))
   (selection
    :accessor selection
    :initarg :selection
    :type list
    :initform nil)))

(defmethod print-object ((include include-instruction) stream)
  (with-slots (file selection)
      include
    (if selection
	(format stream "include(~a,[~{~a~^,~}])." file selection)
	(format stream "include(~a)." file))))

(defmethod render ((include include-instruction))
  (with-slots (file selection)
      include
    (format nil "include(~a,[~{~a~^,~}])." file selection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expanding includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric expand-include (include root-dir))

(defgeneric expand-includes (tptp))

(defmethod expand-includes ((tptp-db tptp-db))
  (let ((new-formulas nil)
	(path (path tptp-db))
	(dir (problem-directory tptp-db)))
    (if (null (include-instructions tptp-db))
	tptp-db
	(loop
	   for formula in (formulas tptp-db)
	   do
	     (if (eql (type-of formula) 'include-instruction)
		 (let ((expanded (expand-include formula dir)))
		   (dolist (x expanded)
		     (push x new-formulas)))
		 (push formula new-formulas))
	   finally
	     (return (make-instance 'tptp-db
				    :formulas (reverse new-formulas)
				    :path path))))))

(defun include-instructions (tptp-db)
  (remove-if-not #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defgeneric has-include-instruction-p (problem)
  (:documentation "Does PROBLEM contain at least one include instruction?"))

(defmethod has-include-instruction-p ((problem tptp-db))
  (some #'(lambda (x) (typep x 'include-instruction)) (formulas problem)))

(defun formulas-w/o-includes (tptp-db)
  (remove-if #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defgeneric axioms (tptp))

(defmethod axioms ((db tptp-db))
  (remove-if-not #'axiom-p (formulas db)))

(defgeneric axiom-p (formula))

(defmethod axiom-p ((formula tptp-formula))
  (if (slot-boundp formula 'source)
      (let ((source (source formula)))
	(cond ((typep source 'internal-source)
	       nil)
	      ((typep source 'general-list)
	       (null (terms source)))
	      ((integerp source)
	       nil)
	      (t
	       (error "Don't know how to determine whether '~a' is an axiom." formula))))
      t))

(defgeneric premises (thing))

(defmethod premises ((x null))
  nil)

(defmethod premises ((x null))
  nil)

(defmethod premises ((x general-list))
  (reduce #'append (mapcar #'premises (terms x))))

(defmethod premises ((x list))
  (reduce #'append (mapcar #'premises x)))

(defmethod premises ((x integer))
  (list x))

(defmethod premises ((x string))
  (list x))

(defmethod premises ((x tptp-formula))
  (when (slot-boundp x 'source)
    (premises (source x))))

(defmethod premises ((x atomic-expression))
  nil)

(defgeneric find-formula (formulas name))

(defmethod find-formula ((formulas null) name)
  nil)

(defmethod find-formula ((formulas list) name)
  (find (stringify name)
	formulas
	:key #'(lambda (x) (stringify (name x)))
	:test #'string=))

(defmethod atomic-formula-p ((x tptp-formula))
  (atomic-formula-p (formula x)))

(defmethod negation-p ((x tptp-formula))
  (negation-p (formula x)))

(defmethod literal-p ((x tptp-formula))
  (literal-p (formula x)))

(defmethod negate :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula) (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula) (optional-info x)))
    new-formula))

(defmethod negate ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (negate (formula x))))

(defgeneric problematize (db)
  (:documentation "Make a single problem formula out of a whole TPTP DB."))

(defmethod problematize ((db tptp-db))
  (if (has-conjecture-p db)
      (let ((c (conjecture-formula db))
            (premises (non-conjecture-formulas db)))
        (setf c (formula c))
        (setf premises (mapcar #'formula premises))
        (if (null premises)
            c
            (make-implication (apply #'make-multiple-arity-conjunction
                                     premises)
                              c)))))

(defmethod contains-contradiction-p ((x tptp-db))
  (some #'contains-contradiction-p (formulas x)))

(defmethod contains-contradiction-p ((x tptp-formula))
  (contains-contradiction-p (formula x)))

(defmethod contains-verum-p ((x tptp-db))
  (some #'contains-verum-p (formulas x)))

(defmethod contains-verum-p ((x tptp-formula))
  (contains-verum-p (formula x)))

(defmethod contains-quantifier-p ((x tptp-db))
  (some #'contains-quantifier-p (formulas x)))

(defmethod contains-quantifier-p ((x tptp-formula))
  (contains-quantifier-p (formula x)))
