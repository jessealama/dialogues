;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dialogue-rule ()
  ((name :initarg :name
	 :accessor name
	 :initform "(no name was supplied)"
	 :type string)
   (description :initarg :description
		:accessor description
		:initform "(no description was supplied)"
		:type string)))

(defmethod print-object ((rule dialogue-rule) stream)
  (print-unreadable-object (rule stream :type t)
    (format stream "~A" (name rule))))

(defmacro make-structural-rule (&key name description predicate)
  (let ((pred-as-fun `(lambda (dialogue &key final-move-only)
			(declare (ignorable dialogue final-move-only))
			,predicate)))
    `(make-instance 'structural-rule
		    :name ,name
		    :description ,description
		    :predicate ,pred-as-fun)))

(defclass particle-rule (dialogue-rule)
  ((precondition :initarg :precondition
		 :accessor precondition
		 :type function
		 :documentation "A function of six arguments (DIALOGUE CURRENT-PLAYER CURRENT-POSITION CURRENT-STATEMENT CURRENT-STANCE CURRENT-REFERENCE).  It if evaluates to NIL when given these inputs, which are understood as a dialogue together with a proposed move to place at the end of the dialogue, it is interpreted as failing to satisfy the condition of the rule.")
   (body :initarg :body
	 :accessor body
	 :type function
	 :documentation "A function of six arguments (DIALOGUE CURRENT-PLAYER CURRENT-POSITION CURRENT-STATEMENT CURRENT-STANCE CURRENT-REFERENCE).  It if evaluates to NIL when given these inputs, which are understood as a dialogue together with a proposed move to place at the end of the dialogue, it is interpreted as satisfying the \"body\" of the rule in question.  The function can assume that the predicate defined in the PRECONDITION slot holds (so that, e.g., certain objects that are tested for existence by the condition can be assumed to exist by this function).")))

(defun particle-rule? (thing)
  (typep thing 'particle-rule))

(defmacro make-particle-rule (&key name description precondition body)
  `(make-instance 'particle-rule
		  :name ,name
		  :description ,description
		  :precondition (lambda (dialogue
					 current-player
					 current-position
					 current-statement
					 current-stance
					 current-reference)
				  (declare (ignorable dialogue
						      current-player
						      current-position
						      current-statement
						      current-stance
						      current-reference))
				  ,precondition)
		  :body (lambda (dialogue
				 current-player
				 current-position
				 current-statement
				 current-stance
				 current-reference)
			  (declare (ignorable dialogue
					      current-player
					      current-position
					      current-statement
					      current-stance
					      current-reference))
			  ,body)))

(defclass structural-rule (dialogue-rule)
  ((predicate :initarg :predicate
	      :type function
	      :accessor predicate)))

(defun structural-rule? (thing)
  (typep thing 'structural-rule))

(defun attack? (stance)
  (eq stance 'a))

(defun defense? (stance)
  (eq stance 'd))

(defmacro make-defensive-rule (&key name
			            (precondition t)
			            body
				    description)
  `(make-particle-rule
    :name ,name
    :precondition (and (defense? current-stance)
		       ,precondition)
    :body ,body
    :description ,description))

(defmacro make-offensive-rule (&key name
			            (precondition t)
			            body
			            description)
  `(make-particle-rule
    :name ,name
    :precondition (and (attack? current-stance)
		       ,precondition)
    :body ,body
    :description ,description))

(defmacro with-original-statement ((original-statement) &body body)
  (let ((attack (gensym))
	(attack-refers-to (gensym))
	(original-move (gensym)))
    `(let ((,attack (nth-move dialogue current-reference)))
       (when ,attack
	 (let ((,attack-refers-to (move-reference ,attack)))
	   (when ,attack-refers-to
	     (let ((,original-move (nth-move dialogue ,attack-refers-to)))
	       (when ,original-move
		 (let ((,original-statement (move-statement ,original-move)))
		   (when ,original-statement
		     ,@body))))))))))
