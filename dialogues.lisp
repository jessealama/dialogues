;;; dialogues.lisp Play Lorenzen dialogue games

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'utils "utils.lisp")
  (require 'formulas "formulas.lisp"))

(defun print-move (move stream depth)
  (declare (ignore depth))
  (let ((stance (move-stance move))
	(ref (move-reference move))
	(statement (move-statement move)))
    (if (and stance ref) ; a non-initial move
	(format stream "[~A,~A] ~A" stance ref statement)
	(format stream "~A (initial move)" statement))))

(defun make-move (player statement stance reference)
  (make-move-int :player player
		 :statement statement
		 :stance stance
		 :reference reference))

(defun make-proponent-move (statement stance reference)
  (make-move 'p statement stance reference))

(defun make-opponent-move (statement stance reference)
  (make-move 'o statement stance reference))

(defun make-attack (player statement reference)
  (make-move player statement 'a reference))

(defun make-defense (player statement reference)
  (make-move player statement 'd reference))

(defun proponent-move? (move)
  (eq (move-player move) 'p))

(defun opponent-move? (move)
  (eq (move-player move) 'o))

(defstruct (move
	     (:print-function print-move)
	     (:constructor make-move-int))
  player
  statement
  stance
  reference)

(defun attacking-move? (move)
  (eq (move-stance move) 'A))

(defun defensive-move? (move)
  (eq (move-stance move) 'D))

(defun initial-move? (move)
  (and (null (move-stance move))
       (null (move-reference move))))

(defun print-initial-move (dialogue stream)
  (let ((first-move (first (dialogue-plays dialogue)))
	 (num-moves (dialogue-length dialogue)))
    (if (= num-moves 1)
	(format stream "0 ~A ~A" (move-player first-move)
		                 (move-statement first-move))
	(let* ((num-digits (ceiling (log num-moves 10)))
	       (padding (make-string (+ 5 num-digits) :initial-element #\Space)))
	  (format stream (concatenate 'string "0 ~A " padding "~A~%")
		         (move-player first-move)
		         (move-statement first-move))))))

(defun print-move-at-position (position move stream)
  (let ((statement (move-statement move))
	(stance (move-stance move))
	(ref-index (move-reference move)))
    (if (evenp position)
	(format stream "~A P [~A,~A] ~A" position
		                         stance
					 ref-index
					 statement)
	(format stream "~A O [~A,~A] ~A" position 
	                                 stance
					 ref-index
					 statement))))

(defun print-dialogue (dialogue stream depth)
  (declare (ignore depth))
  (let ((plays (dialogue-plays dialogue)))
    (cond ((null plays) (format stream ""))
	  (t 
	   (print-initial-move dialogue stream)
	   (when (cdr plays)
	     (do ((i 1 (1+ i))
		  (moves (cddr plays) (cdr moves))
		  (move (second plays) (car moves)))
		 ((null moves) (print-move-at-position i move stream))
	       (print-move-at-position i move stream)
	       (format stream "~%")))))))

(defun make-dialogue (initial-statement signature)
  (let ((first-move (make-proponent-move initial-statement nil nil)))
    (make-dialogue-int :plays (list first-move)
		       :signature signature)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun some-move (predicate dialogue)
  (some predicate (dialogue-plays dialogue)))

(defun every-move (predicate dialogue)
  (every predicate (dialogue-plays dialogue)))

(defun nth-move (dialogue n)
  (nth n (dialogue-plays dialogue)))

(defun last-move (dialogue)
  (nth-move dialogue (1- (dialogue-length dialogue))))

(defun nth-statement (dialogue n)
  (move-statement (nth-move dialogue n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar symbolic-attacks
  '(attack-left-conjunct attack-right-conjunct which-instance? which-disjunct?))

(defun symbolic-attack? (obj)
  (member obj symbolic-attacks))

(defun statement? (obj signature)
  (or (formula? signature obj)
      (symbolic-attack? obj)
      (term? signature obj)))

(defun read-statement (signature)
  (let (response)
    (until (statement? response signature)
      (setf response (read t nil nil)))
    response))

(defun read-statement-or-symbols (signature &rest symbols)
  (let (response)
    (until (or (statement? signature response)
	       (member response symbols))
      (setf response (read t nil nil)))
    response))

(defun read-formula-or-term (signature)
  (let (response)
    (until (or (formula? signature response)
	       (term? signature response))
      (setf response (read t nil nil)))
    response))

(defun non-symbolic-attack-term? (obj signature)
  "Determine whether OBJ is a term different from the symbolic
attacks which, being symbols, do qualify as terms."
  (and (not (symbolic-attack? obj))
       (term? signature obj)))

(defun non-symbolic-attack-formula? (obj signature)
  "Determine whether OBJ is a formula different from the symbolic
  attacks which, being simply lisp symbols, do qualify as [atomic]
  formulas)."
  (and (not (symbolic-attack? obj))
       (formula? signature obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argumentation forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-rule (&key name condition body failure-message)
  (let ((condition-result (gensym))
	(condition-error (gensym))
	(body-result (gensym))
	(body-error (gensym)))
  `(lambda (dialogue current-player 
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
     (with-value-and-error (,condition-result ,condition-error)
         ,condition
       (if ,condition-error
	   (warn "An error occurred while evaluating the condition for rule ~A!~%The type of the error was ~A.~%The dialogue at this point is:~%~A~%player: ~A~%position: ~A~%statement: ~A~%stance: ~A~%reference:~A~%Continuing..." 
		 (quote ,name)
		 ,condition-error
		 dialogue
		 current-player
		 current-position
		 current-statement
		 current-stance
		 current-reference)
	   (if ,condition-result
	       (with-value-and-error (,body-result ,body-error)
		   ,body
		 (if ,body-error 
		     (warn "An error occurred while evaluating the body of rule ~A!~%The type of the error was ~A.~%The dialogue at this point is:~%~A~%player: ~A~%position: ~A~%statement: ~A~%stance: ~A~%reference:~A~%Continuing..." 
			   (quote ,name)
			   ,body-error
			   dialogue
			   current-player
			   current-position
			   current-statement
			   current-stance
			   current-reference)
		     (values ,body-result (format nil (concatenate 'string "[~A] " ,failure-message) (quote ,name)))))
	       (values t nil)))))))

(defmacro make-defensive-rule (&key name
			            (condition t)
			            body
				    failure-message)
  `(make-rule :name ,name
	      :condition (and (defense? current-stance)
			      ,condition)
	      :body ,body
	      :failure-message ,failure-message))

(defmacro make-offensive-rule (&key name
			            (condition t)
			            body
			            failure-message)
  `(make-rule :name ,name
	      :condition (and (attack? current-stance)
			      ,condition)
	      :body ,body
	      :failure-message ,failure-message))

(defmacro with-original-statement ((original-statement) &body body)
  (let ((attack (gensym))
	(attack-refers-to (gensym))
	(original-move (gensym)))
    `(let* ((,attack (nth-move dialogue current-reference))
	    (,attack-refers-to (move-reference ,attack))
	    (,original-move (nth-move dialogue ,attack-refers-to))
	    (,original-statement (move-statement ,original-move)))
       ,@body)))

(defvar rule-d01-conjunction
  (make-offensive-rule
   :name d01-conjunction
   :condition (conjunction? (nth-statement dialogue current-reference))
   :body (or (eq current-statement 'attack-left-conjunct)
	     (eq current-statement 'attack-right-conjunct))
   :failure-message "Only two attacks against conjunctions are permitted:~%ATTACK-LEFT-CONJUNCT and ATTACK-RIGHT-CONJUNCT."))

(defvar rule-d01-left-conjunct
  (make-offensive-rule
   :name d01-left-conjunct
   :condition (eq current-statement 'attack-left-conjunct)
   :body (and (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
	      (conjunction? (nth-statement dialogue current-reference)))
   :failure-message "One cannot attack the left conjunct of a formula~%that isn't a conjunction."))

(defvar rule-d01-right-conjunct
  (make-offensive-rule
   :name d01-right-conjunct
   :condition (eq current-statement 'attack-right-conjunct)
   :body (and (non-symbolic-attack-formula?
	       (nth-statement dialogue current-reference)
	       (dialogue-signature dialogue))
	      (conjunction? (nth-statement dialogue current-reference)))
   :failure-message "One cannot attack the right conjunct of a formula that isn't a conjunction."))

(defvar rule-d01-disjunction
  (make-offensive-rule
   :name d01-disjunction
   :condition (disjunction? (nth-statement dialogue current-reference))
   :body (eq current-statement 'which-disjunct?)
   :failure-message "WHICH-DISJUNCT? is the only permissible attack against a disjunction."))

(defvar rule-d01-which-disjunct
  (make-offensive-rule
   :name d01-which-disjunct
   :condition (eq current-statement 'which-disjunct?)
   :body (disjunction? (nth-statement dialogue current-reference))
   :failure-message "The WHICH-DISJUNCT? attack applies only to disjunctions."))

(defvar rule-d01-implication
  (make-offensive-rule
   :name d01-implication
   :condition (implication? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
	      (equal-formulas? current-statement
			       (antecedent
				(nth-statement dialogue current-reference))))
   :failure-message "To attack an implication, one must assert the antecdent."))

(defvar rule-d01-negation
  (make-offensive-rule 
   :name d01-negation
   :condition (negation? (nth-statement dialogue current-reference))
   :body (and (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
	      (equal-formulas? current-statement
			       (unnegate
				(nth-statement dialogue current-reference))))
   :failure-message "To attack a negation, one must assert the \"unnegation\" of the negation."))

(defvar rule-d01-universal
  (make-offensive-rule
   :name d01-universal
   :condition (universal? (nth-statement dialogue current-reference))
   :body (non-symbolic-attack-term? current-statement
				    (dialogue-signature dialogue))
   :failure-message "To attack a universal, one must assert a term."))

(defvar rule-d01-term
  (make-offensive-rule
   :name d01-term
   :condition (non-symbolic-attack-term? current-statement
					 (dialogue-signature dialogue))
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s
					      (dialogue-signature dialogue))
		(universal? s)))
   :failure-message "If one asserts a term as an attack, then the assertion being attacked must be a universal generalization."))

(defvar rule-d01-which-instance
  (make-offensive-rule 
   :name d01-which-instance
   :condition (eq current-statement 'which-instance?)
   :body (let ((s (nth-statement dialogue current-reference)))
	   (and (non-symbolic-attack-formula? s
					      (dialogue-signature dialogue))
		(existential? s)))
   :failure-message "The WHICH-INSTANCE? attack applies only to existential generalizations."))

(defvar rule-d01-existential
  (make-offensive-rule
   :name d01-existential
   :condition (existential? (nth-statement dialogue current-reference))
   :body (eq current-statement 'which-instance?)
   :failure-message "WHICH-INSTANCE? is the only permissible attack on existential generalizations."))

(defvar rule-d01-formula
  (make-offensive-rule 
   :name d01-formula
   :condition (non-symbolic-attack-formula? current-statement
					    (dialogue-signature dialogue))
   :body (or (implication? (nth-statement dialogue current-reference))
	     (negation? (nth-statement dialogue current-reference))
	     (universal? (nth-statement dialogue current-reference)))
   :failure-message "When the attacking statement is a formula,~%the statement being attacked must be either~%an implication or a negation."))

(defvar rule-d02-formula
  (make-defensive-rule
   :name d02-formula
   :body (non-symbolic-attack-formula? current-statement
				       (dialogue-signature dialogue))
   :failure-message "All defensive statements are supposed to be formulas."))

(defvar rule-d02-left-conjunct
  (make-defensive-rule
   :name d02-left-conjunct
   :condition (eq (nth-statement dialogue current-reference)
		  'attack-left-conjunct)
  :body (with-original-statement (original-statement)
	  (equal-formulas? current-statement
			   (left-conjunct original-statement)))
  :failure-message "To defend against the ATTACK-LEFT-CONJUNCT attack,~% assert the left conjunct of the original conjunction."))

(defvar rule-d02-right-conjunct
  (make-defensive-rule 
   :name d02-right-conjunct
   :condition (eq (nth-statement dialogue current-reference)
		  'attack-right-conjunct)
   :body (with-original-statement (original-statement)
	   (equal-formulas? current-statement
			    (right-conjunct original-statement)))
   :failure-message "To defend against the ATTACK-RIGHT-CONJUNCT attack,~% assert the right conjunct of the original conjunction."))

(defvar rule-d02-which-disjunct
  (make-defensive-rule
   :name d02-which-disjunct
   :condition (eq (nth-statement dialogue current-reference) 'which-disjunct?)
   :body (with-original-statement (original-statement)
	   (or (equal-formulas? current-statement
				(left-disjunct original-statement))
	       (equal-formulas? current-statement
				(right-disjunct original-statement))))
   :failure-message "To defend against the WHICH-DISJUNCT? attack,~%assert either the left or the right disjunct~%of the original disjunction."))

(defvar rule-d02-implication
  (make-defensive-rule 
   :name d02-implication
   :condition (with-original-statement (original-statement)
		(implication? original-statement))
   :body (with-original-statement (original-statement)
	   (equal-formulas? current-statement
			    (consequent original-statement)))
   :failure-message "To defend against an attack on an implication, assert its consequent."))

(defvar rule-d02-negation
  (make-defensive-rule 
   :name d02-negation
   :condition (with-original-statement (original-statement)
		(negation? original-statement))
   :body t
   :failure-message "One cannot (directly) defend against an attack on a negation."))

(defvar rule-d02-universal
  (make-defensive-rule
   :name d02-universal
   :condition (with-original-statement (original-statement)
		(universal? original-statement))
   :body (let* ((attack (nth-move dialogue current-reference))
		(attack-refers-to (move-reference attack))
		(instance (move-statement attack))
		(original-move (nth-move dialogue attack-refers-to))
		(original-statement (move-statement original-move))
		(var (bound-variable original-statement))
		(matrix (matrix original-statement)))
	   (equal-formulas? current-statement
			    (instantiate instance var matrix)))
   :failure-message "The asserted statement is not the required instance of the original universal generalization."))

(defvar rule-d02-existential
  (make-defensive-rule
   :name d02-existential
   :condition (with-original-statement (original-statement)
		(existential? original-statement))
   :body (with-original-statement (original-statement)
	   (instance-of-quantified? current-statement
				    original-statement
				    (dialogue-signature dialogue)))
   :failure-message "The asserted statement is not an instance of the original existential generalization."))

(defvar argumentation-forms (list rule-d01-conjunction
				  rule-d01-left-conjunct
				  rule-d01-right-conjunct
				  rule-d01-disjunction
				  rule-d01-which-disjunct
				  rule-d01-implication
				  rule-d01-negation
				  rule-d01-universal
				  rule-d01-term
				  rule-d01-which-instance
				  rule-d01-existential
				  rule-d01-formula
				  rule-d02-formula
				  rule-d02-left-conjunct
				  rule-d02-right-conjunct
				  rule-d02-which-disjunct
				  rule-d02-implication
				  rule-d02-negation
				  rule-d02-universal
				  rule-d02-existential))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attack? (sym)
  (eq sym 'a))

(defun defense? (sym)
  (eq sym 'd))

(defvar rule-d00-atomic
  (make-rule :name d00-atomic
	     :condition (zerop current-position)
	     :body (composite-formula? current-statement
				       (dialogue-signature dialogue))
	     :failure-message "Dialogues must open with a composite formula."))

(defvar rule-d00-proponent
  (make-rule :name d00-proponent
	     :condition (evenp current-position)
	     :body (eq current-player 'p)
	     :failure-message "Proponent plays even-numbered positions.  (Counting starts at zero.)"))

(defvar rule-d00-opponent
  (make-rule :name d00-opponent
	     :condition (oddp current-position)
	     :body (eq current-player 'o)
	     :failure-message "Opponent plays odd-numbered positions.  (Counting starts at zero.)"))

(defvar rule-d01-composite
  (make-offensive-rule
   :name d01-composite
   :body (composite-formula? (nth-statement dialogue 
					    current-reference)
			     (dialogue-signature dialogue))
   :failure-message "Atomic formulas cannot be attacked."))

(defvar rule-d02-attack
  (make-defensive-rule 
   :name d02-attack
   :body (attacking-move? (nth-move dialogue current-reference))
   :failure-message "The move being defended against is not an attack."))

(defvar rule-d10
  (make-rule :name d10
	     :condition (and (evenp current-position) 
			     (non-symbolic-attack-formula? current-statement
							   (dialogue-signature dialogue))
			     (atomic-formula? current-statement
					      (dialogue-signature dialogue)))
	     :body (some-move #'(lambda (move)
				  (when (opponent-move? move)
				    (equal-formulas? (move-statement move)
						     current-statement)))
			      dialogue)
	     :failure-message "Proponent cannot assert an atomic formula before opponent has asserted it."))

(defun attacking-moves (dialogue)
  (remove-if-not #'attacking-move? (dialogue-plays dialogue)))

(defun defensive-moves (dialogue)
  (remove-if-not #'defensive-move? (dialogue-plays dialogue)))

(defun closed-attack-indices (dialogue)
  (mapcar #'move-reference (defensive-moves dialogue)))

(defun open-attack-indices (dialogue)
  (let ((moves (dialogue-plays dialogue)))
    (when moves
      (do* ((i 0 (1+ i))
	    (move (car moves) (car moves-tail))
	    (moves-tail (cdr moves) (cdr moves-tail))
	    (result (when (attacking-move? move)
		      (list 0))
		    (if (attacking-move? move)
			(cons i result)
			(remove (move-reference move) result))))
	   ((null moves-tail) result)))))

(defun most-recent-open-attack (dialogue)
  (let ((open-attacks (open-attack-indices dialogue)))
    (when open-attacks
      (car open-attacks))))

(defvar rule-d11
  (make-defensive-rule 
   :name d11
   :body (let ((most-recent (most-recent-open-attack dialogue)))
	   (or (null most-recent)
	       (= most-recent current-reference)))
   :failure-message "You must defend against only the most recent open attack."))

(defvar rule-d12
  (make-defensive-rule
   :name d12
   :body (every-move #'(lambda (move)
			 (or (initial-move? move)
			     (attacking-move? move)
			     (/= (move-reference move)
				 current-reference)))
		     dialogue)
   :failure-message "Attacks may be answered at most once."))

(defvar rule-d13
  (make-offensive-rule
   :name d13
   :condition (oddp current-position)
   :body (every-move #'(lambda (move)
			 (or (proponent-move? move)
			     (/= (move-reference move)
				 current-reference)))
		     dialogue)
   :failure-message "A P-assertion may be attacked at most once."))

(defvar rule-e
  (make-rule :name e
	     :condition (oddp current-position)
	     :body (= current-reference (1- current-position))
	     :failure-message "Opponent must react to the most recent statement by Proponent."))

(defvar d-dialogue-rules (append argumentation-forms 
				 (list rule-d00-atomic
				       rule-d00-proponent
				       rule-d00-opponent
				       rule-d01-composite
				       rule-d02-attack
				       rule-d10
				       rule-d11
				       rule-d12
				       rule-d13)))

(defvar e-dialogue-rules (append d-dialogue-rules (list rule-e)))
	
(defstruct (dialogue
	     (:print-function print-dialogue)
	     (:constructor make-dialogue-int))
  (signature nil :type signature)
  (plays nil :type list))

(defun dialogue-length (dialogue)
  (length (dialogue-plays dialogue)))

(defun next-moves (dialogue)
  "The set of moves by which DIALOGUE can be legally extended."
  (declare (ignore dialogue))
  nil)

(defun proponent-wins? (dialogue)
  (let ((len (dialogue-length dialogue)))
    (when (evenp len)
      (null (next-moves dialogue)))))

(defun add-move-to-dialogue (dialogue move)
  (setf (dialogue-plays dialogue)
	(append (dialogue-plays dialogue) 
		(list move))))

(defun evaluate-rules (rules dialogue player turn-number statement stance index)
  (if (null rules)
      (values t nil)
      (let ((rule (car rules)))
	(multiple-value-bind (result error-message)
	    (funcall rule dialogue player turn-number statement stance index)
	  (if result
	      (evaluate-rules (cdr rules) dialogue
			                  player
					  turn-number 
					  statement 
					  stance 
					  index)
	      (values nil error-message))))))

(defvar *signature* nil)

(defun read-signature ()
  (let (constants predicates functions)
    (tagbody (go constants)
     constants
       (msg "Do you want to input any constant symbols?~%")
       (ecase (read-symbol 'y 'n 'yes 'no)
	 (y (go first-constant))
	 (yes (go first-constant))
	 (n (go functions))
	 (no (go functions)))
     first-constant
       (msg "Input a symbol:~%")
       (push (read-symbol) constants)
       (msg "Enter more constants?")
       (ecase (read-symbol 'y 'n 'yes 'no)
	 (y (go more-constants))
	 (yes (go more-constants))
	 (n (go functions))
	 (no (go functions)))
     more-constants
       (msg "You've declared ~A constants so far ~A~%" (length constants)
                                                       constants)
       (msg "Do you want to input any more?")
       (let ((response (read-symbol 'y 'n 'yes 'no)))
	 (ecase response
	   (y (msg "Input a symbol:~%")
	      (push (read-symbol) constants)
	      (go more-constants))
	   (yes (msg "Input a symbol:~%")
		(push (read-symbol) constants)
		(go more-constants))
	   (n (go functions))
	   (no (go functions))))
     functions
       (msg "Do you want to input any function symbols?~%")
       (ecase (read-symbol 'y 'n 'yes 'no)
	 (y (go first-function))
	 (yes (go first-function))
	 (n (go predicates))
	 (no (go predicates)))
     first-function
       (let (func-sym arity)
	 (msg "Input a symbol:~%")
	 (setf func-sym (read-symbol))
	 (msg "What arity does ~A have?~%" func-sym)
	 (setf arity (read-natural-number))
	 (push (cons func-sym arity) functions)
	 (msg "Enter more function symbols?")
	 (ecase (read-symbol 'y 'n 'yes 'no)
	   (y (go more-functions))
	   (yes (go more-functions))
	   (n (go predicates))
	   (no (go predicates))))
     more-functions
       (msg "You've declared ~A function symbols so far ~A~%" (length functions)
                                                              functions)
       (msg "Do you want to input any more?")
       (let ((response (read-symbol 'y 'n 'yes 'no)))
	 (ecase response
	   (y (let (func-sym arity)
		(msg "Input a symbol:~%")
		(setf func-sym (apply #'read-symbol-different-from
				      (append constants
					      functions)))
		(msg "What arity does ~A have?" func-sym)
		(setf arity (read-natural-number))
		(push (cons func-sym arity) functions)
		(go more-functions)))
	   (yes (let (func-sym arity)
		(msg "Input a symbol:~%")
		(setf func-sym (apply #'read-symbol-different-from
				      (append constants
					      functions)))
		(msg "What arity does ~A have?" func-sym)
		(setf arity (read-natural-number))
		(push (cons func-sym arity) functions)
		(go more-functions)))
	   (n (go predicates))
	   (no (go predicates))))
     predicates
       (msg "Do you want to input any predicates?~%")
       (ecase (read-symbol 'y 'n 'yes 'no)
	 (y (go first-predicate))
	 (yes (go first-predicate))
	 (n (go check))
	 (no (go check)))
     first-predicate
       (let (pred-sym arity)
	 (msg "Input a symbol:~%")
	 (setf pred-sym (read-symbol))
	 (msg "What arity does ~A have?~%" pred-sym)
	 (setf arity (read-natural-number))
	 (push (cons pred-sym arity) predicates)
	 (msg "Enter more predicates?")
	 (ecase (read-symbol 'y 'n 'yes 'no)
	   (y (go more-predicates))
	   (yes (go more-predicates))
	   (n (go check))
	   (no (go check))))
     more-predicates
       (msg "You've declared ~A predicates so far ~A~%" (length functions)
                                                        functions)
       (msg "Do you want to input any more?")
       (let ((response (read-symbol 'y 'n 'yes 'no)))
	 (ecase response
	   (y (let (pred-sym arity)
		(msg "Input a symbol:~%")
		(setf pred-sym (apply #'read-symbol-different-from
				      (append constants
					      predicates
					      functions)))
		(msg "What arity does ~A have?" pred-sym)
		(setf arity (read-natural-number))
		(push (cons pred-sym arity) functions)
		(go more-predicates)))
	   (yes (let (pred-sym arity)
		(msg "Input a symbol:~%")
		(setf pred-sym (apply #'read-symbol-different-from 
				      (append constants
					      predicates
					      functions)))
		(msg "What arity does ~A have?" pred-sym)
		(setf arity (read-natural-number))
		(push (cons pred-sym arity) predicates)
		(go more-predicates)))
	   (n (go check))
	   (no (go check))))
     check
       (when predicates
	 (msg "The signature looks like this:~%")
	 (msg "Constants: ~A~%" constants)
	 (msg "Predicates: ~A~%" predicates)
	 (msg "Functions: ~A~%" functions)
	 (msg "Do you want to add to this?~%")
	 (ecase (read-symbol 'y 'n 'yes 'no)
	   (y (go end))
	   (yes (go end))
	   (n (msg "OK, returning to the first prompt...~%")
	      (go constants))
	   (no (msg "OK, returning to the first prompt...~%")
	       (go constants))))
       (msg "No predicates have been entered; you won't be able to say anything!~%")
       (msg "Returning to the first prompt...~%")
       (go constants)
     end)
    (make-signature :predicates predicates
		    :functions functions
		    :constants constants)))

(defun play-dialogue-game (rules)
  (let ((dialogue nil)
	(turn-number 0)
	(signature nil)
	(player nil)
	(stance nil)
	(index nil)
	(statement nil))
    (tagbody (go greetings)
     greetings
       (msg "Let's play a dialogue game!~%")
       (when (null *signature*)
	 (go signature))
       (msg "The global variable *SIGNATURE* has already been set; reusing it.")
       (go initial-move)
     signature
       (msg "Please supply a signature in which the sentences will be written.~%")
       (setf signature (read-signature))
       (go initial-move)
     initial-move
       (msg "Proponent starts by playing a composite formula.~%")
       (msg "Input a composite formula: ")
       (setf dialogue (make-dialogue (read-composite-formula signature)
				     signature))
       (msg "Game on!~%")
       (incf turn-number)
       (go start-move)
     start-move
       (if (evenp turn-number)
	   (setf player 'p)
	   (setf player 'o))
       (msg "Turn #~A: ~A~%" turn-number (ecase player
					   (p "Proponent")
					   (o "Opponent")))
       (msg "Enter:~%")
       (msg "- A to attack,~%")
       (msg "- D to defend,~%")
       (msg "- P to print the dialogue so far,~%")
       (msg "- Q to quit.~%")
       (ecase (read-symbol 'a 'd 'p 'q)
	 (q (go quit))
	 (p (go print-then-restart))
	 (a (go attack))
	 (d (go defend)))
     print-then-restart
       (msg "The dialogue so far looks like this:~%")
       (msg "~A~%" dialogue)
       (go start-move)
     print-then-attack
       (msg "The dialogue so far looks like this:~%")
       (msg "~A~%" dialogue)
       (go attack)
     print-then-defend
       (msg "The dialogue so far looks like this:~%")
       (msg "~A~%" dialogue)
       (go defend)
     print-then-statement
       (msg "The dialogue so far looks like this:~%")
       (msg "~A~%" dialogue)
       (go statement)
     print-then-statement-input
       (msg "The dialogue so far looks like this:~%")
       (msg "~A~%" dialogue)
       (go statement-input)
     attack
       (msg "Attack which move? Enter:~%")
       (msg "- An integer between 0 and ~A,~%" (1- turn-number))
       (msg "- P to print the dialogue so far and come back to this prompt,~%")
       (msg "- Q to quit,~%")
       (msg "- R to restart the move.~%")
       (setf index (read-number-in-interval-or-symbol 
		    0 (1- turn-number) 
		    'p 'q 'r))
       (when (integerp index)
	 (setf stance 'a)
	 (go statement))
       (ecase index
	 (p (go print-then-attack))
	 (q (go quit))
	 (r (go start-move)))
     defend
       (msg "Defend against which move? Enter:~%")
       (msg "- An integer between 0 and ~A,~%" (1- turn-number))
       (msg "- P to print the dialogue so far and come back to this prompt,~%")
       (msg "- Q to quit,~%")
       (msg "- R to restart the move.~%")
       (setf index (read-number-in-interval-or-symbol 
		    0 (1- turn-number) 
		    'p 'q 'r))
       (when (integerp index)
	 (setf stance 'd)
	 (go statement))
       (ecase index
	 (p (go print-then-defend))
	 (q (go quit))
	 (r (go start-move)))
     formula-input
       (msg "Enter a formula:~%")
       (setf statement (read-formula (dialogue-signature dialogue)))
       (go evaluate-rules)
     term-input
       (msg "Enter a term:~%")
       (setf statement (read-term))
       (go evaluate-rules)
     statement-input
       (if (eq stance 'a)
	   (msg "What is your attack? ")
	   (msg "What is your defense? "))
       (msg "Enter:~%")
       (msg "- P to print the dialogue so far and return to this prompt,~%")
       (msg "- Q to quit,~%")
       (msg "- F to type a formula,~%")
       (msg "- T to type a term,~%")
       (when (eq stance 'a)
	 (msg "- L for ATTACK-LEFT-CONJUNCT,~%")
	 (msg "- R for ATTACK-RIGHT-CONJUNCT,~%")
	 (msg "- D for WHICH-DISJUNCT?,~%")
	 (msg "- I for WHICH-INSTANCE?,~%"))
       (ecase (read-symbol 'p 'q 'f 't 'l 'r 'd 'i)
	 (p (go print-then-statement-input))
	 (f (go formula-input))
	 (t (go term-input))
	 (l (setf statement 'attack-left-conjunct))
	 (r (setf statement 'attack-right-conjunct))
	 (d (setf statement 'which-disjunct?))
	 (i (setf statement 'which-instance?)))
       (go evaluate-rules)
     statement
       (msg "You are responding to move #~A.  Enter:~%" index)
       (msg "- P to print the dialogue so far and come back to this prompt,~%")
       (msg "- Q to quit,~%")
       (msg "- R to restart the move,~%")
       (msg "- S to enter your response to move #~A.~%" index)
       (setf statement (read-symbol 'p 'q 'r 's))
       (ecase statement
	 (p (go print-then-statement))
	 (q (go quit))
	 (r (go start-move))
	 (s (go statement-input)))
     evaluate-rules
       (multiple-value-bind (rules-result message)
	   (evaluate-rules rules dialogue player turn-number statement stance index)
	 (when rules-result
	   (go successful-turn))
	 (msg "At least one of the dialogue rules is violated by your attack.~%")
	 (msg "The rule says:~%~A~%" message)
	 (msg "Restarting the move...~%")
	 (go start-move))
     successful-turn
       (incf turn-number)
       (add-move-to-dialogue dialogue
			     (make-move player statement stance index))
       (go start-move)
     quit
       (msg "Thanks for playing, I hope you had fun.~%"))
    dialogue))

(defun play-d-dialogue-game ()
  (play-dialogue-game d-dialogue-rules))

(defun play-e-dialogue-game ()
  (play-dialogue-game e-dialogue-rules))

(provide 'dialogues)

;;; dialogues.lisp ends here