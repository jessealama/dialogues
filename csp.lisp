;;; csp.lisp: Constraint satisfaction problems

(require 'search "search.lisp")

(defstruct csp-problem
"A constraint satisfaction problem involves filling in values for its
variables.  We will use a csp-state structure to represent this.  To
specify a csp problem, we indicate whether we should carry out forward
checking (indicated by FORWARD-CHECKING?), and a unary function,
VARIABLE-SELECTOR, that determines which variable should be selected
next."
  (constraints nil)
  (variables nil)
  (forward-checking? nil)      ; should we filter domains?
  (domain-function)
  (variable-selector #'first-unassigned-variable)
  (domain-ordering #'domain-of-variable)
  )

;;; All csps use integers as names for both variables and their values.
;;; Constraints on variables var1, var2 are represented by a table,
;;; indexed by var1, var2, with each entry a list of all allowable pairs
;;; of values for var1, var2.
(defstruct csp-state
  unassigned                   ;; variables that have not been given values
  assigned                     ;; variables with known values
  constraint-fn                ;; checks allowed pairwise assignments
  modified                     ;; variable modified to make this state
  )

(defstruct (csp-var (:type list))
  name
  domain
  value
  conflicts)

;;;; Generic Functions for csp Problems

(defun csp-legal-statep (s)
  (every #'(lambda (var1)
	     (every #'(lambda (var2)
			(funcall (csp-state-constraint-fn s)
				 (csp-var-name var1) (csp-var-value var1)
				 (csp-var-name var2) (csp-var-value var2)))
		    (cdr (member var1 (csp-state-assigned s) :test #'eq))))
	 (csp-state-assigned s)))

(defun filter-domains (name value unassigned constraint-fn)
  (mapcar #'(lambda (var)
	      (let ((name2 (csp-var-name var))
		    (domain (csp-var-domain var)))
		(make-csp-var :name name2
			      :domain (remove-if-not
				       #'(lambda (val2)
					   (funcall constraint-fn
						    name value name2 val2))
				       domain))))
	  unassigned))

(defun csp-legal-assignmentp (name value assigned constraint-fn)
  (every #'(lambda (var)
	     (funcall constraint-fn name value 
		                    (csp-var-name var) (csp-var-value var)))
	 assigned))

(defun eval-in-environment (expression environment)
  (cond ((eq expression 'true) t)
	((eq expression 'false) nil)
	((numberp expression) expression)
	((symbolp expression)
	 (let ((val (assoc expression environment)))
	   (if val
	       (cdr val)
	       (error "Unable to evaluate ~S in environment ~S"
		      expression environment))))
	((listp expression)
	 (let ((first (first expression)))
	   (cond ((eq first '=)
		  (equal (eval-in-environment (second expression) environment)
			 (eval-in-environment (third expression) environment)))
		 ((eq first 'not)
		  (not (eval-in-environment (second expression) environment)))
		 ((eq first '+)
		  (apply #'+ (mapcar #'(lambda (exp)
					 (eval-in-environment exp environment))
				     (cdr expression))))
		 ((eq first '*)
		  (apply #'* (mapcar #'(lambda (exp)
					 (eval-in-environment exp environment))
				     (cdr expression))))
		 ((eq first '-)
		  (apply #'- (mapcar #'(lambda (exp)
					 (eval-in-environment exp environment))
				     (cdr expression))))
		 ((eq first 'abs)
		  (abs (eval-in-environment (second expression) environment)))
		 (t
		  (error "Unable to evaluate ~S in environment ~S"
			 expression environment)))))
	(t (error "Unable to evaluate ~S in environment ~S"
		  expression environment))))

(defun variable? (obj)
  (and (symbolp obj)
       (not (eq obj '=))
       (not (eq obj '+))
       (not (eq obj '-))
       (not (eq obj '*))
       (not (eq obj 'not))
       (not (eq obj 'abs))))

(defun assignment-variables (assignment)
  (mapcar #'car assignment))

(defun constraint-variables (constraint)
  (remove-if-not #'variable? (flatten constraint))) 

(defun assignment-satisfies-constraint? (assignment constraint)
  (let ((constraint-variables (constraint-variables constraint))
	(assignment-variables (assignment-variables assignment)))
    (or (some #'(lambda (var) (not (member var assignment-variables)))
	      constraint-variables)
	(eval-in-environment constraint assignment))))

(defun csp-consistent-assignment? (assignment problem)
  "Determine whether ASSIGNMENT is consistent with the constraints
given by PROBLEM."
  (or (null assignment)
      (let ((constraints (csp-problem-constraints problem)))
	(every #'(lambda (constraint)
		   (assignment-satisfies-constraint? assignment constraint))
	       constraints))))

(defun first-unassigned-variable (assignment csp-problem)
  (do* ((assignment-vars (assignment-variables assignment))
	(vars (csp-problem-variables csp-problem) (cdr vars))
	(var (car vars) (car vars)))
       ((not (member var assignment-vars)) var)))

(defun domain-of-variable (variable assignment csp-problem)
  (declare (ignore assignment))
  (let ((domain-function (csp-problem-domain-function csp-problem)))
    (funcall domain-function variable)))

(defun extend-assignment (assignment variable value)
  (cons (cons variable value) assignment))
  
(defun csp-assignment-consistently-extendable? (assignment var val csp-problem)
  "Determine whether ASSIGNMENT can be consistently extended by
  setting variable VAR to the value VAL."
  (csp-consistent-assignment? (extend-assignment assignment var val) 
			      csp-problem))

(defun goal-test (node-or-state)
  "NODE-OR-STATE is a goal if all variables are assigned legally."
  (let ((state (if (node-p node-or-state)
		   (node-state node-or-state) 
		   node-or-state)))
    (and (null (csp-state-unassigned state));; The state is legal
	 (csp-legal-statep state))))

(defun assignment-complete? (assignment csp-problem)
  (let ((problem-variables (csp-problem-variables csp-problem))
	(assignment-variables (assignment-variables assignment)))
    (and (subsetp problem-variables assignment-variables)
	 (subsetp assignment-variables problem-variables))))

(defun csp-backtracking-search (assignment csp-problem)
  (if (assignment-complete? assignment csp-problem)
      assignment
      (let ((vs (csp-problem-variable-selector csp-problem))
	    (dof (csp-problem-domain-ordering csp-problem)))
	(let ((variable (funcall vs assignment csp-problem)))
	  (or (loop for value in (funcall dof variable assignment csp-problem) do
		   (if (csp-assignment-consistently-extendable?
			assignment
			variable 
			value
			csp-problem)
		       (let ((result (csp-backtracking-search
				      (extend-assignment assignment variable value)
				      csp-problem)))
			 (if (not (eq result 'fail))
			     (return result)))))
	      'fail)))))

(defun csp-backtracking-search-with-counts (assignment csp-problem)
  (labels ((backtrack (a p)
	     (if (assignment-complete? a p)
		 assignment
		 (let ((vs (csp-problem-variable-selector csp-problem))
		       (dof (csp-problem-domain-ordering csp-problem)))
		   (let ((variable (funcall vs assignment csp-problem)))
		     (or (loop for value in (funcall dof variable assignment csp-problem) do
			      (if (csp-assignment-consistently-extendable?
				   assignment
				   variable 
				   value
				   csp-problem)
				  (let ((result (csp-backtracking-search
						 (extend-assignment assignment variable value)
						 csp-problem)))
				    (if (not (eq result 'fail))
					(return result)))))
			 'fail)))))


(provide 'csp)

;;; csp.lisp ends here
