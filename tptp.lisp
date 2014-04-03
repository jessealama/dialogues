
(in-package :dialogues)

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
  (format stream "fof")
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

(defgeneric make-tptp-formula (thing))

(defmethod make-tptp-formula ((thing list))
  (destructuring-bind (syntax name status formula . more-stuff)
      thing
    (if more-stuff
	(destructuring-bind (source . useful-info)
	    more-stuff
	  (make-instance 'tptp-formula
		   :name (if (symbolp name)
			     (symbol-name name)
			     (format nil "~a" name))
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula)
		   :source source
		   :useful-info useful-info))
	(make-instance 'tptp-formula
		   :name (if (symbolp name)
			     (symbol-name name)
			     (format nil "~a" name))
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula)))))

(defun sort-formula-list (formula-list)
  (let ((sorted (sort formula-list #'string< :key #'name)))
    sorted))

(defparameter *tptp-to-lisp-stylesheet*
  #p"/Users/alama/sources/xsl4tptp/tptp-to-lisp.xsl")

(defgeneric xmlize-tptp (tptp))

(defmethod xmlize-tptp :around ((tptp-file pathname))
  (if (probe-file tptp-file)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-file))))

(defmethod xmlize-tptp ((tptp-file pathname))
  (let ((tptp4X-out (make-string-output-stream))
	(tptp4X-err (make-string-output-stream))
	(tptp-dir (pathname (directory-namestring tptp-file))))
    (with-current-directory (tptp-dir)
      (run-program "tptp4X"
		   (list "-c" "-x" "-fxml" "--")
		   :wait t
		   :output tptp4X-out
		   :error tptp4X-err
		   :input tptp-file))
    (prog1
	(get-output-stream-string tptp4X-out)
      (close tptp4X-out)
      (close tptp4X-err))))

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

(defmethod signature ((formula tptp-formula))
  (signature (formula formula)))

(defmethod signature ((tptp tptp-db))
  (reduce #'merge-signatures
	  (mapcar #'signature
		  (mapcar #'formula
			  (formulas (expand-includes tptp))))))

(defclass derivability-problem (tptp-db)
  ((conjecture
    :initarg :conjecture
    :accessor conjecture
    :initform (error "To specify a derivability problem, a conjecture must be supplied."))))

(defmethod print-object ((problem derivability-problem) stream)
  (let ((conjecture (conjecture problem))
	(formulas (formulas problem)))
    (format stream "~a" conjecture)
    (when formulas
      (terpri stream)
      (format stream "~{~a~^~%~}" formulas))))

(defmethod initialize-instance :after ((problem derivability-problem) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (conjecture-formula (premises problem))
    (error "Some non-conjecture formula has the TPTP status 'conjecture'."))
  (loop
     :initially (setf (role (conjecture problem)) "conjecture")
     :for formula in (formulas problem)
     :for role = (role formula)
     :unless (string= role "conjecture") :do (setf (role formula) "axiom")
     :finally (return problem)))

(defgeneric make-derivability-problem (formulas))

(defmethod make-derivability-problem ((formulas tptp-db))
  (let ((conjecture (conjecture-formula formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas (non-conjecture-formulas formulas)
		       :conjecture conjecture
		       :path (path formulas))
	(error "There is no conjecture formula in ~a." formulas))))

(defmethod make-derivability-problem ((formulas null))
  (error "The empty list does not contain a conjecture formula."))

(defmethod make-derivability-problem ((formulas list))
  (let ((conjecture (find "conjecture" formulas :test #'string= :key #'role))
	(non-conjecture-formulas (remove-if #'(lambda (formula)
						(string= (role formula) "conjecture"))
					    formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas non-conjecture-formulas
		       :conjecture conjecture)
	(error "No conjecture formula found in ~{~a~%~}" formulas))))

(defmethod make-derivability-problem ((problem pathname))
  (make-derivability-problem (parse-tptp problem)))

(defmethod render ((formulas list))
  (if formulas
      (format nil "~{~a~%~}" (mapcar #'render formulas))
      (format nil "(empty formula list)")))

(defmethod render ((problem tptp-db))
  (render (formulas problem)))

(defmethod render ((problem derivability-problem))
  (with-output-to-string (s)
    (dolist (formula (formulas problem))
      (format s "~a" (render formula))
      (terpri s))
    (format s "~a" (render (conjecture problem)))
    (terpri s)))

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

(defun remove-conjecture (problem)
  (make-instance 'tptp-db
		 :formulas (remove (conjecture-formula problem)
				   (formulas problem))))

(defgeneric remove-formula (formulas formula))

(defmethod remove-formula ((formulas tptp-db) (formula-name string))
  "Remove any formula in FORMULAS whose name is FORMULA-NAME."
  (make-instance 'tptp-db
		 :formulas (remove formula-name
				   (formulas formulas)
				   :test (lambda (x y) (string= (stringify x)
                                                                (stringify y)))
				   :key #'name)))

(defmethod remove-formula (formulas (formula-name number))
  "Remove any formula in FORMULAS whose name is FORMULA-NAME."
  (remove-formula formulas (stringify formula-name)))

;; (defmethod remove-formula ((problem derivability-problem) (formula tptp-formula))
;;   (let ((name-to-remove (name formula))
;; 	(conjecture-name (name (conjecture problem))))
;;     (if (string= name-to-remove conjecture-name)
;; 	(make-instance 'tptp-db
;; 		       :formulas (formulas problem))
;; 	(make-instance 'derivability-problem
;; 		       :conjecture (conjecture problem)
;; 		       :formulas (remove-if #'(lambda (x) (string= x name-to-remove))
;; 					    (formulas problem)
;; 					    :key #'name)))))

(defmethod remove-formula ((formulas tptp-db) (formula tptp-formula))
  (remove-formula formulas (name formula)))

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

(defgeneric change-status (formula new-status))

(defmethod change-status :around ((formula tptp-formula) status)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula) (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod change-status ((formula tptp-formula) new-status)
  (make-instance (class-of formula)
		 :name (name formula)
		 :syntax (role formula)
		 :role new-status
		 :formula (formula formula)))

(defgeneric change-status-of-formula-in (formula problem new-status)
  (:documentation "Change the TPTP status of FORMULA in PROBLEM to NEW-STATUS."))

(defmethod change-status-of-formula-in ((formula string)
					(problem pathname)
					(new-status string))
  (change-status-of-formula-in formula (parse-tptp problem) new-status))

(defmethod change-status-of-formula-in ((formula string)
					(problem tptp-db)
					(new-status string))
  (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas)))))))

(defmethod change-status-of-formula-in ((formula string)
					(problem derivability-problem)
					(new-status string))
  (if (string= new-status "conjecture")
      (let ((conjecture (conjecture problem)))
	(let ((conjecture-name (name conjecture)))
	  (if (string= conjecture-name formula)
	      problem
	      (error "The given derivability-problem already has a conjecture formula; (by the name ~a), so we cannot change the status of ~a into 'conjecture'." conjecture-name formula))))
      (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas))))))))

(defun promote-conjecture-to-axiom (problem)
  (let ((conjecture (has-conjecture-p problem)))
    (if conjecture
	(make-instance 'tptp-db
		       :formulas (cons (change-status conjecture "axiom")
				       (non-conjecture-formulas problem)))
	problem)))

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

(defmethod formula-with-name ((tptp-path pathname) name)
  (formula-with-name (parse-tptp tptp-path) name))

(defgeneric premises (problem))

(defmethod premises ((db tptp-db))
  (non-conjecture-formulas db))

(defgeneric restrict-to (db formulas)
  (:documentation "Restrict DB to the formulas in FORMULAS."))

(defmethod restrict-to ((db tptp-db) (formulas list))
  (let ((new-formulas nil))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name db formula)))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name db (name formula))))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'tptp-db
		   :formulas new-formulas)))

(defmethod restrict-to ((problem derivability-problem) (formulas list))
  (let* ((new-formulas nil)
	 (conjecture (conjecture problem))
	 (conjecture-name (name conjecture)))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name problem formula)))
	       (when formula-in-db
		 (unless (string= formula conjecture-name)
		   (push formula-in-db new-formulas)))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name problem (name formula))))
	       (when formula-in-db
		 (unless (string= (name formula) conjecture-name)
		   (push formula-in-db new-formulas)))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'derivability-problem
		   :conjecture conjecture
		   :formulas new-formulas)))

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

(defmethod expand-include ((include include-instruction) root-dir)
  (with-slots (selection file)
      include
    (when (null selection)
      (return-from expand-include nil))
    (let ((real-file nil)
	  (new-formulas nil))
      (cond ((file-exists-p file)
	     (setf real-file file))
	    ((directory-exists-p root-dir)
	     (let ((file-in-dir (merge-pathnames file root-dir)))
	       (if (file-exists-p file-in-dir)
		   (setf real-file file-in-dir)
		   (error "No file found at '~a', nor could we find '~a'." (namestring file) (namestring file-in-dir)))))
	    ((null root-dir)
	     (error "No file at '~a', and no root directory was supplied." (namestring file)))
	    (t
	     (error "No file at '~a', and to boot a bogus directory (~a) was supplied." (namestring file) root-dir)))
      (let ((included-db (handler-case (parse-tptp real-file)
			   (error () nil))))
	(unless included-db
	  (error "Error parsing '~a' as a TPTP file." (namestring real-file)))
	(setf included-db (expand-includes included-db))
	(loop
	   for selected in selection
	   for corresponding = (formula-with-name included-db
						  selected)
	   do
	     (when (null corresponding)
	       (error "There is no formula in the TPTP problem '~a' with the name '~a'." (namestring real-file) selected))
	     (push corresponding new-formulas)
	   finally
	     (return (reverse new-formulas)))))))

(defgeneric expand-includes (tptp))

(defmethod expand-includes ((tptp-db pathname))
  (expand-includes (parse-tptp tptp-db)))

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

(defmethod has-include-instruction-p ((problem pathname))
  (has-include-instruction-p (parse-tptp problem)))

(defmethod has-include-instruction-p ((problem tptp-db))
  (some #'(lambda (x) (typep x 'include-instruction)) (formulas problem)))

(defun formulas-w/o-includes (tptp-db)
  (remove-if #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defgeneric exists-path (from to thing))

(defmethod exists-path (from to (db tptp-db))
  (exists-path from to (dependency-table db)))

(defmethod exists-path (from (to integer) thing)
  (exists-path from (format nil "~a" to) thing))

(defmethod exists-path ((from integer) to thing)
  (exists-path (format nil "~a" from) to thing))

(defmethod exists-path (from (to symbol) thing)
  (exists-path from (symbol-name to) thing))

(defmethod exists-path ((from symbol) to thing)
  (exists-path (symbol-name from) to thing))

(defmethod exists-path ((from string) (to string) (table hash-table))
  (if (string= from to)
      (list from)
      (loop
	 :with predecessors = (gethash from table)
	 :for pred :in predecessors
	 :for path = (exists-path pred to table)
	 :when path :do (return (cons from path))
	 :finally (return nil))))

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

(defgeneric easily-equivalent (formula-1 formula-2 background-premises)
  (:documentation "Can we quickly prove that FORMULA-1 and FORMULA-2
  are logically equivalent (in first-order classical logic with
  identity?  BACKGROUND-PREMISES is a list of formula objects relative to which FORMULA-1 and FORMULA-2 will be checked for equivalence."))

(defmethod easily-equivalent ((formula-1 tptp-formula) (formula-2 tptp-formula) background-premises)
  (easily-equivalent (formula formula-1)
		     (formula formula-2)
		     background-premises))

(defmethod easily-equivalent ((formula-1 string) formula-2 background-premises)
  (easily-equivalent (parse-tptp formula-1)
		     formula-2
		     background-premises))

(defmethod easily-equivalent (formula-1 (formula-2 string) background-premises)
  (easily-equivalent formula-1 (parse-tptp formula-2) background-premises))

(defmethod easily-equivalent (formula-1 (formula-2 tptp-db) background-premises)
  (easily-equivalent formula-1
		     (make-instance 'multiple-arity-conjunction
				    :items (mapcar #'formula (formulas formula-2)))
		     background-premises))

(defmethod easily-equivalent ((formula-1 tptp-db) formula-2 background-premises)
  (easily-equivalent (make-instance 'multiple-arity-conjunction
				    :items (mapcar #'formula (formulas formula-1)))
		     formula-2
		     background-premises))

(defparameter *easily-equivalent-timeout* 5
  "The maximum number of seconds we will spend trying to prove that two formulas are equivalent.")

(defmethod easily-equivalent ((formula-1 formula) (formula-2 formula) background-premises)
  (let ((formula-1-closed (universally-close formula-1))
	(formula-2-closed (universally-close formula-2)))
    (let ((temp-problem (temporary-file :base "problem"))
	  (temp-output (temporary-file :base "output")))
      (let ((premises-string (if background-premises
				 (with-output-to-string (s)
				   (dolist (x background-premises)
				     (format s "~a~%" (strip-source (universally-close x)))))
				 "")))
	(let ((problem (format nil "~a~%fof(equivalent,conjecture,(~a <=> ~a)).~%"
			       premises-string
			       formula-1-closed
			       formula-2-closed)))
	  (write-string-into-file problem temp-problem)))
      (run-program "eproof"
		   (list "--auto"
			 "--tstp-format"
			 "--memory-limit=1024"
			 (format nil "--cpu-limit=~d" *easily-equivalent-timeout*)
			 (native-namestring temp-problem))
		   :search t
		   :input nil
		   :output temp-output
		   :wait t)
      (prog1
	  (not (null (scan "SZS status Theorem" (file-contents temp-output))))
	(when (file-exists-p temp-output)
	  (delete-file temp-output))
	(when (file-exists-p temp-problem)
	  (delete-file temp-problem))))))

(defgeneric fofify (tptp))

(defmethod fofify ((l list))
  (mapcar #'fofify l))

(defmethod fofify ((x null))
  nil)

(defmethod fofify ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'fofify (formulas db))))

(defmethod fofify :around ((formula tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula) (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod fofify ((x formula))
  (universally-close x))

(defmethod fofify ((formula tptp-formula))
  (make-instance 'fof
		 :name (name formula)
		 :role (role formula)
		 :formula (fofify (formula formula))))

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

(defun replace-premises (formula new-premises)
  (setf (source formula)
	(make-instance 'atomic-expression
		       :head (intern "inference" :dialogues)
		       :arguments (list (make-instance 'atomic-expression
						       :head (intern "unknown" :dialogues)
						       :arguments nil)
					(make-instance 'general-list)
					(make-instance 'general-list
						       :terms new-premises))))
  formula)

(defgeneric find-formula (formulas name))

(defmethod find-formula ((formulas null) name)
  nil)

(defmethod find-formula ((formulas list) name)
  (find (stringify name)
	formulas
	:key #'(lambda (x) (stringify (name x)))
	:test #'string=))

(defgeneric update-inference-parents (thing from to additional-premises)
  (:documentation "In THING, update any reference to FROM in an
  inference record to TO, as well as inserting ADDITIONAL-PREMISES (if present) that were possibly used to show that FROM and TO are equivalent."))

(defmethod update-inference-parents ((thing null) from to additional-premises)
  nil)

(defmethod update-inference-parents ((l general-list) from to additional-premises)
  (make-instance 'general-list
		 :terms (mapcar #'(lambda (term)
				    (update-inference-parents term
							      from
							      to
							      additional-premises))
				(terms l))))

(defmethod update-inference-parents ((formula-list list) from to additional-premises)
  (mapcar #'(lambda (x)
	      (update-inference-parents x from to additional-premises))
	  formula-list))

(defmethod update-inference-parents :around ((x tptp-formula) from to additional-premises)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod update-inference-parents ((x tptp-formula) from to additional-premises)
  (if (slot-boundp x 'source)
      (make-instance (class-of x)
		     :name (name x)
		     :role (role x)
		     :formula (formula x)
		     :source (update-inference-parents (source x)
						       from
						       to
						       additional-premises))
      x))

(defmethod update-inference-parents ((db tptp-db) from to additional-premises)
  (make-instance (class-of db)
		 :path (path db)
		 :formulas (mapcar #'(lambda (x)
				       (update-inference-parents x
								 from
								 to
								 additional-premises))
				   (formulas db))))

(defmethod update-inference-parents ((x atomic-expression) from to additional-premises)
  (with-slots (head arguments)
      x
    (if (and (string= (stringify head) "file")
	     (length= 2 arguments))
	x ;; this looks like an external source
	(make-instance (class-of x)
		       :head head
		       :arguments (mapcar #'(lambda (arg)
					      (update-inference-parents arg
									from
									to
									additional-premises))
					  arguments)))))

(defmethod update-inference-parents ((x string) from to additional-premises)
  (if (string= x (stringify from))
      to
      x))

(defmethod update-inference-parents ((x integer) from to additional-premises)
  (if (string= (format nil "~d" x)
	       (stringify from))
      to
      x))

(defmethod update-inference-parents ((x symbol) from to additional-premises)
  (if (string= (symbol-name x)
	       (stringify from))
      to
      x))

(defgeneric reduce-equivalences (tptp premises &key predicate))

(defmethod reduce-equivalences ((l null) premises &key predicate)
  (declare (ignore premises predicate))
  nil)

(defmethod reduce-equivalences (x (premise tptp-formula) &key predicate)
  (reduce-equivalences x (formula premise) :predicate predicate))

(defmethod reduce-equivalences (x (premise formula) &key predicate)
  (reduce-equivalences x (list premise) :predicate predicate))

(defmethod reduce-equivalences (x (db tptp-db) &key predicate)
  (reduce-equivalences x (formulas db) :predicate predicate))

(defun first-trivially-equivalent-pair (formula-list)
  (loop
     :for i :from 0
     :for x :in formula-list
     :for n = (position-if #'(lambda (y)
			       (let ((premises (premises y)))
				 (when (and (length= 1 premises)
					    (find (stringify (name x))
						  premises
						  :key #'stringify
						  :test #'string=))
				   (easily-equivalent x
						      y
						      nil))))
			   formula-list
			   :start (1+ i)
			   :from-end t)
     :when n :do (return (cons i n))
     :finally (return (cons nil nil))))

(defun first-equivalent-pair (formula-list background-premises &key predicate)
  (unless predicate
    (setf predicate (constantly t)))
  (loop
     :for i :from 0
     :for x :in formula-list
     :do
     (when (funcall predicate x)
       (let ((n (position-if #'(lambda (y)
				 (when (find (stringify (name x))
					     (premises y)
					     :key #'stringify
					     :test #'string=)
				   (when (funcall predicate y)
				     (easily-equivalent x
							y
							background-premises))))
			     formula-list
			     :start (1+ i)
			     :from-end t)))
	 (when n
	   (return (cons i n)))))
     :finally (return (cons nil nil))))

(defmethod reduce-trivial-equivalences ((l list))
  (format t "Reducing trivial equivalences in a proof having ~a steps~%" (length l))
  (destructuring-bind (index-to-keep . index-to-remove)
      (first-trivially-equivalent-pair l)
    (if (and index-to-keep index-to-remove)
	(let ((formula-to-keep (nth index-to-keep l))
	      (formula-to-remove (nth index-to-remove l))
	      (before (subseq l 0 index-to-remove))
	      (after (subseq l (1+ index-to-remove))))
	  (let ((name-to-keep (name formula-to-keep))
		(name-to-remove (name formula-to-remove)))
	    (let ((updated-after (update-inference-parents after
							   name-to-remove
							   name-to-keep
							   nil)))
	      (reduce-trivial-equivalences (append before updated-after)))))
	l)))

(defmethod reduce-trivial-equivalences ((db pathname))
  (reduce-trivial-equivalences (parse-tptp db)))

(defmethod reduce-trivial-equivalences ((db tptp-db))
  (make-instance 'tptp-db
                 :formulas (reduce-trivial-equivalences (formulas db))))

(defmethod reduce-equivalences ((l list) (background-premises list) &key predicate)
  (format t "Reducing a proof having ~a steps using ~d extra premises ~%" (length l) (length background-premises))
  (unless predicate
    (setf predicate (constantly t)))
  (destructuring-bind (index-to-keep . index-to-remove)
      (first-equivalent-pair l background-premises :predicate predicate)
    (if (and index-to-keep index-to-remove)
	(let ((formula-to-keep (nth index-to-keep l))
	      (formula-to-remove (nth index-to-remove l))
	      (before (subseq l 0 index-to-remove))
	      (after (subseq l (1+ index-to-remove))))
	  (let ((name-to-keep (name formula-to-keep))
		(name-to-remove (name formula-to-remove)))
	    (let ((updated-after (update-inference-parents after
							   name-to-remove
							   name-to-keep
							   background-premises)))
	      (reduce-equivalences (append before updated-after)
				   background-premises
				   :predicate predicate))))
	l)))

(defmethod reduce-equivalences ((db tptp-db) background-premises &key predicate)
  (reduce-equivalences (formulas db) background-premises :predicate predicate))

(defgeneric confirm-premise-minimality (tptp-thing background)
  (:documentation "Confirm that the premises of TPTP-THING are
  minimized.  BACKGROUND specifies the ambient set of all available
  premises."))

(defmethod confirm-premise-minimality ((db tptp-db) dummy)
  (declare (ignore dummy))
  (with-slots (formulas)
      db
    (loop
       :for i :from 0
       :for formula :in formulas
       :for name = (name formula)
       :do
       (format t "Checking minimality of ~a...~%" name)
       (multiple-value-bind (minimized index-of-removable-premise)
	   (confirm-premise-minimality formula db)
	 (declare (ignore index-of-removable-premise))
	 (unless minimized
	   (return (values nil i))))
       :finally
       (return (values t nil)))))

(defmethod confirm-premise-minimality ((x tptp-formula) (db tptp-db))
  (if (slot-boundp x 'source)
      (with-slots (source name formula)
	  x
	(let ((premises (premises x)))
	  (let ((premise-formulas (mapcar #'(lambda (x) (formula-with-name db x))
					  premises))
		(conjecture (make-instance 'fof
					   :name name
					   :role "conjecture"
					   :formula (fofify formula)
					   :source source)))
	    (loop
	       :for i :from 0
	       :for premise in premise-formulas
	       :for other-formulas = (append (subseq premise-formulas 0 i)
					     (subseq premise-formulas (1+ i)))
	       :for problem = (make-instance 'derivability-problem
					     :conjecture conjecture
					     :formulas (mapcar #'fofify other-formulas))
	       :for solution = (solve-problem problem :timeout 10)
	       :when (szs-implies? solution "Theorem") :do (return (values nil i))
	       :finally (return (values t nil))))))
      (values t nil)))

(defun extract-problem (formula db)
  (if (slot-boundp formula 'source)
      (let ((premises (premises formula)))
	(let ((premise-formulas (mapcar #'(lambda (x) (formula-with-name db x))
					premises))
	      (conjecture (make-instance 'fof
					 :name (name formula)
					 :role "conjecture"
					 :formula (fofify (formula formula)))))
	  (make-instance 'derivability-problem
			 :conjecture conjecture
			 :formulas (mapcar #'strip-source
					   (mapcar #'fofify premise-formulas)))))
      (make-instance 'derivability-problem
		     :conjecture (make-instance 'fof
						:name (name formula)
						:role "conjecture"
						:formula (fofify (formula formula)))
		     :formulas nil)))

(defgeneric strip-optional-info (tptp-thing))

(defmethod strip-optional-info ((x null))
  nil)

(defmethod strip-optional-info ((l list))
  (mapcar #'strip-optional-info l))

(defmethod strip-optional-info ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'strip-optional-info (formulas db))))

(defmethod strip-optional-info :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    new-formula))

(defmethod strip-optional-info ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (formula x)))


(defgeneric strip-source (tptp-thing))

(defmethod strip-source ((x null))
  nil)

(defmethod strip-source ((l list))
  (mapcar #'strip-source l))

(defmethod strip-source ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'strip-source (formulas db))))

(defmethod strip-source ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (formula x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming predicate and function symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rename-symbol (tptp old-name new-name)
  (:documentation "In TPTP, rename the function/predicate symbol whose name is OLD-NAME by NEW-NAME."))

(defmethod rename-symbol ((db tptp-db) old-name new-name)
  (make-instance 'tptp-db
		 :formulas (mapcar #'(lambda (x)
				       (rename-symbol x old-name new-name))
				   (formulas db))))

(defmethod rename-symbol :around ((x tptp-formula) old-name new-name)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod rename-symbol ((x tptp-formula) old-name new-name)
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (rename-symbol (formula x) old-name new-name)))

(defmethod rename-symbol ((atom atomic-formula) old-name new-name)
  (with-slots (predicate arguments)
      atom
    (make-instance (class-of atom)
		   :predicate (if (string= (stringify predicate)
					   (stringify old-name))
				  (intern (stringify new-name) :dialogues)
				  predicate)
		   :arguments (mapcar #'(lambda (x)
					  (rename-symbol x old-name new-name))
				      arguments))))

(defmethod rename-symbol ((atom atomic-expression) old-name new-name)
  (with-slots (head arguments)
      atom
    (make-instance (class-of atom)
		   :head (if (string= (stringify head)
				      (stringify old-name))
			     (intern (stringify new-name) :dialogues)
			     head)
		   :arguments (mapcar #'(lambda (x)
					  (rename-symbol x old-name new-name))
				      arguments))))

(defmethod rename-symbol ((neg negation) old-name new-name)
  (make-instance 'negation
		 :argument (rename-symbol (argument neg) old-name new-name)))

(defmethod rename-symbol ((x binary-connective-formula) old-name new-name)
  (make-instance (class-of x)
		 :lhs (rename-symbol (lhs x) old-name new-name)
		 :rhs (rename-symbol (rhs x) old-name new-name)))

(defmethod rename-symbol ((x multiple-arity-connective-formula) old-name new-name)
  (make-instance (class-of x)
		 :items (mapcar #'(lambda (x) (rename-symbol x old-name new-name))
				(items x))))

(defmethod rename-symbol ((gen generalization) old-name new-name)
  (make-instance (class-of gen)
		 :bindings (bindings gen)
		 :matrix (rename-symbol (matrix gen) old-name new-name)))

(defmethod rename-symbol ((eq equation) old-name new-name)
  (make-instance 'equation
		 :lhs (rename-symbol (lhs eq) old-name new-name)
		 :rhs (rename-symbol (rhs eq) old-name new-name)))


(defmethod rename-symbol ((eq disequation) old-name new-name)
  (make-instance 'disequation
		 :lhs (rename-symbol (lhs eq) old-name new-name)
		 :rhs (rename-symbol (rhs eq) old-name new-name)))

(defmethod rename-symbol ((var variable-term) old-name new-name)
  var)

(defmethod flatten-conjunctions/disjunctions ((x null))
  nil)

(defmethod flatten-conjunctions/disjunctions ((l list))
  (mapcar #'flatten-conjunctions/disjunctions l))

(defmethod flatten-conjunctions/disjunctions ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'flatten-conjunctions/disjunctions (formulas db))))

(defmethod flatten-conjunctions/disjunctions :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod flatten-conjunctions/disjunctions ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (flatten-conjunctions/disjunctions (formula x))))

(defmethod terms-with-functor (functor-name (db tptp-db))
  (terms-with-functor functor-name (formulas db)))

(defmethod terms-with-functor (functor-name (x tptp-formula))
  (terms-with-functor functor-name (formula x)))

(defmethod eliminate-truth-values ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'eliminate-truth-values (formulas db))))

(defmethod eliminate-truth-values :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod eliminate-truth-values ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (eliminate-truth-values (formula x))))

(defgeneric dependency-table (tptp)
  (:documentation "A hash table whose keys are strings naming formulas of TPTP and whose values for a key X is the list of one-step dependencies of X."))

(defmethod dependency-table ((db tptp-db))
  (loop
     :with dep-table = (make-hash-table :test #'equal)
     :for formula :in (formulas db)
     :for name = (stringify (name formula))
     :for premises = (premises formula)
     :do (setf (gethash name dep-table) (mapcar #'stringify premises))
     :finally (return dep-table)))

(defmethod universally-close :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod universally-close ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (universally-close (formula x))))

(defmethod contains-predicate? ((x tptp-formula) predicate)
  (contains-predicate? (formula x) predicate))

(defmethod atomic-formula-p ((x tptp-formula))
  (atomic-formula-p (formula x)))

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

(defgeneric definition-p (x)
  (:documentation "Is X a definition?"))

(defmethod definition-p ((x tptp-formula))
  (string= (role x) "definition"))

(defgeneric hypothesis-p (x)
  (:documentation "Is X a hypothesis?"))

(defmethod hypothesis-p ((x tptp-formula))
  (string= (role x) "hypothesis"))

(defgeneric save-to-file (tptp-thing destination &key if-exists)
  (:documentation "Save TPTP-THING to DESTINATION.  IF-EXISTS should be a keyword that has the same meaning as the IF-EXISTS keyword in WITH-OPEN-FILE."))

(defmethod save-to-file (db (path string) &key (if-exists :error))
  (save-to-file db (pathname path) :if-exists if-exists))

(defmethod save-to-file ((db tptp-db) (path pathname) &key (if-exists :error))
  (with-open-file (file path
                        :direction :output
                        :if-exists if-exists
                        :if-does-not-exist :create)
    (format file "~{~a~^~%~}" (formulas db)))
  t)

(defgeneric problematize (db)
  (:documentation "Make a single problem formula out of a whole TPTP DB."))

(defmethod problematize ((path pathname))
  (problematize (parse-tptp path)))

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
