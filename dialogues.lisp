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


(defun make-dialogue (initial-statement)
  (let ((first-move (make-proponent-move initial-statement nil nil)))
    (make-dialogue-int :plays (list first-move))))

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
;;; Argumentation forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe-adheres-to-argumentation-forms (move-1 move-2)
  "Determine whether MOVE-2 could follow MOVE-1 according to the
argumentation forms.

The argumentation forms are, strictly speaking, triples; it does not
make sense to say of, two moves, that they adhere to the
argumentation forms.  What is computed by this function is a derived
notion of adherence: MAYBE-ADHERES-TO-ARGUMENTATION-FORMS determines
whether there could exist a third move, say, MOVE-3, such
that (MOVE-1,MOVE-2,MOVE-3) adheres to the argumentation forms."
  (let ((statement-1 (move-statement move-1))
	(statement-2 (move-statement move-2))
	(player-1 (move-player move-1))
	(player-2 (move-player move-2))
	(stance (move-stance move-2)))
    (and (not (eq player-1 player-2))
	 (eq stance 'a)
	 (cond ((conjunction? statement-1)
		(or (eq statement-2 'attack-left-conjunct)
		    (eq statement-2 'attack-right-conjunct)))
	       ((disjunction? statement-1)
		(eq statement-2 'which-disjunct?))
	       ((implication? statement-1)
		(equal-formulas? statement-2 (antecedent statement-1)))
	       ((negation? statement-1)
		(equal-formulas? statement-2 (unnegate statement-1)))
	       ((universal? statement-1)
		(term? statement-2))
	       ((existential? statement-2)
		(eq statement-2 'which-instance?))
	       (t (error "Unrecognized statement: ~A" statement-1))))))

(defun adheres-to-argumentation-forms (move-1 move-2 move-3)
  "Determine whether the ordered triple (MOVE-1, MOVE-2, MOVE-3)
adheres to the argumentation forms."
  (let ((statement-1 (move-statement move-1))
	(statement-2 (move-statement move-2))
	(statement-3 (move-statement move-3))
	(player-1 (move-player move-1))
	(player-2 (move-player move-2))
	(player-3 (move-player move-3))
	(stance-2 (move-stance move-2))
	(stance-3 (move-stance move-3)))
    (and (eq player-1 player-3)
	 (not (eq player-1 player-2))
	 (eq stance-2 'a)
	 (eq stance-3 'd)
	 (cond ((conjunction? statement-1)
		(or (and (eq statement-2 'attack-left-conjunct)
			 (equal-formulas? statement-3
					  (left-conjunct statement-1)))
		    (and (eq statement-2 'attack-right-conjunct)
			 (equal-formulas? statement-3 
					  (right-conjunct statement-1)))))
	       ((disjunction? statement-1)
		(and (eq statement-2 'which-disjunct?)
		     (or (equal-formulas? statement-3 
					  (left-disjunct statement-1))
			 (equal-formulas? statement-3 
					  (right-conjunct statement-1)))))
	       ((implication? statement-1)
		(and (equal-formulas? statement-2 (antecedent statement-1))
		     (equal-formulas? statement-3 (consequent statement-1))))
	       ((negation? statement-1)
		(equal-formulas? statement-2 (unnegate statement-1)))
	       ((universal? statement-1)
		(when (term? statement-2)
		  (equal-formulas? statement-3 
				   (instantiate statement-2
						(bound-variable statement-1)
						(matrix statement-1)))))
	       ((existential? statement-2)
		(and (eq statement-2 'which-instance?)
		     (instance-of-quantified? statement-3 statement-1)))
	       (t (error "Unrecognized statement: ~A" statement-1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogue rules
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

(defvar rule-d00-atomic
  (make-rule :name d00-atomic
	     :condition (zerop current-position)
	     :body (composite-formula? current-statement)
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
  (make-rule :name d01-composite
	     :condition (eq current-stance 'a)
	     :body (composite-formula? (nth-statement dialogue 
						      current-reference))
	     :failure-message "Atomic formulas cannot be attacked."))

(defvar rule-d01-conjunction
  (make-rule :name d01-conjunction
	     :condition (and (eq current-stance 'a)
			     (conjunction? 
			      (nth-stacement dialogue current-reference)))
	     :body (or (eq current-statement 'attack-left-conjunct)
		       (eq current-statement 'attack-right-conjunct))
	     :failure-message "Only two attacks against conjuncts are permitted: ATTACK-LEFT-CONJUNCT and ATTACK-RIGHT-CONJUNCT."))

(defvar rule-d01-left-conjunct
  (make-rule :name d01-left-conjunct
	     :condition (and (eq current-stance 'a)
			     (eq current-statement 'attack-left-conjunct))
	     :body (and (formula? current-statement)
			(conjunction? (nth-stacement dialogue current-reference)))
	     :failure-message "One cannot attack the left conjunct of something that isn't a conjunction."))

(defvar rule-d01-right-conjunct
  (make-rule :name d01-right-conjunct
	     :condition (and (eq current-stance 'a)
			     (eq current-statement 'attack-right-conjunct))
	     :body (and (formula? current-statement)
			(conjunction? (nth-statement dialogue current-reference)))
	     :failure-message "One cannot attack the left conjunct of something that isn't a conjunction."))

(defvar rule-d01-disjunction
  (make-rule :name d01-disjunction
	     :condition (and (eq current-stance 'a)
			     (disjunction?
			      (nth-statement dialogue current-reference)))
	     :body (eq current-statement 'which-disjunct?)
	     :failure-message "WHICH-DISJUNCT? is the only permissible attack against a disjunction."))

(defvar rule-d01-which-disjunct
  (make-rule :name d01-which-disjunct
	     :condition (and (eq current-stance 'a)
			     (eq current-statement 'which-disjunct?))
	     :body (disjunction? (nth-statement dialogue current-reference))
	     :failure-message "The WHICH-DISJUNCT? attack applies only to disjunctions."))

(defvar rule-d01-implication
  (make-rule :name d01-implication
	     :condition (and (eq current-stance 'a)
			     (implication? (nth-statement current-reference)))
	     :body (and (formula? current-statement)
			(equal-formulas? current-statement
					 (antecedent
					  (nth-statement current-reference))))
	     :failure-condition "To attack an implication, one must assert the antecdent."))

(defvar rule-d01-negation
  (make-rule :name d01-negation
	     :condition (and (eq current-stance 'a)
			     (negation? (nth-statement current-reference)))
	     :body (and (formula? current-statement)
			(equal-formulas? current-statement
					 (unnegate
					  (nth-statement dialogue current-reference))))
	     :failure-message "To attack a negation, one must assert the \"unnegation\" of the negation."))

(defvar rule-d01-universal
  (make-rule :name d01-universal
	     :condition (and (eq current-stance 'a)
			     (universal? (nth-statement dialogue current-reference)))
	     :body (term? current-statement)
	     :failure-message "To attack a universal, one must assert a term."))

(defvar rule-d01-term
  (make-rule :name d01-term
	     :condition (and (eq current-stance 'a)
			     (term? current-statement))
	     :body (let ((s (nth-statement dialogue current-reference)))
		     (and (formula? s)
			  (universal? s)))
	     :failure-message "If one asserts a term as an attack, then the assertion being attacked must be a universal generalization."))

(defvar rule-d01-which-instance
  (make-rule :name d01-which-instance
	     :condition (and (eq current-stance 'a)
			     (eq current-statement 'which-instance?))
	     :body (let ((s (nth-statement dialogue current-reference)))
		     (and (formula? s)
			  (existential? s)))
	     :failure-message "The WHICH-INSTANCE? attack applies only to existential generalizations."))

(defvar rule-d01-existential
  (make-rule :name d01-existential
	     :condition (and (eq current-stance 'a)
			     (existential? (nth-statement dialogue current-reference)))
	     :body (eq current-statement 'which-instance?)
	     :failure-message "WHICH-INSTANCE? is the only permissible attack on existential generalizations."))

(defvar rule-d01-formula
  (make-rule :name d01-formula
	     :condition (and (eq current-stance 'a)
			     (formula? current-statement))
	     :body (or (implication? (nth-statement dialogue current-reference))
		       (negation? (nth-statement dialogue current-reference))
		       (universal? (nth-statement current-reference)))
	     :formula "When the attacking statement is a formula,~%the statement being attacked must be either~%an implication or a negation."))

(defvar rule-d02-attack
  (make-rule :name d02-attack
	     :condition (eq current-stance 'd)
	     :body (attacking-move? (nth-move dialogue current-reference))
	     :failure-message "The move being defended against is not an attack."))

(defvar rule-d02-formula
  (make-rule :name d02-formula
	     :condition (eq current-stance 'd)
	     :body (formula? current-statement)
	     :failure-message "All defensive statements are supposed to be formulas."))

(defvar rule-d02-left-conjunct
  (make-rule :name d02-left-conjunct
	     :condition (and (eq current-stance 'd)
			     (eq (nth-statement dialogue current-reference)
				 'attack-left-conjunct))
	     :body (let* ((attack (nth-move dialogue current-reference))
			  (attack-refers-to (move-reference attack))
			  (original-move (nth-move dialogue attack-refers-to))
			  (original-statement (move-statement original-move)))
		     (equal-formulas? current-statement
				      (left-conjunct original-statement)))
	     :failure-message "To defend against the ATTACK-LEFT-CONJUNCT attack~%, assert the left conjunct of the original conjunction."))

(defvar rule-d02-right-conjunct
  (make-rule :name d02-right-conjunct
	     :condition (and (eq current-stance 'd)
			     (eq (nth-statement dialogue current-reference)
				 'attack-right-conjunct))
	     :body (let* ((attack (nth-move dialogue current-reference))
			  (attack-refers-to (move-reference attack))
			  (original-move (nth-move dialogue attack-refers-to))
			  (original-statement (move-statement original-move)))
		     (equal-formulas? current-statement
				      (right-conjunct original-statement)))
	     :failure-message "To defend against the ATTACK-RIGHT-CONJUNCT attack~%, assert the right conjunct of the original conjunction."))

(defvar rule-d02-which-disjunct
  (make-rule :name d02-which-disjunct
	     :condition (and (eq current-stance 'd)
			     (eq (nth-statement dialogue current-reference)
				 'which-disjunct?))
	     :body (let* ((attack (nth-move dialogue current-reference))
			  (attack-refers-to (move-reference attack))
			  (original-move (nth-move dialogue attack-refers-to))
			  (original-statement (move-statement original-move)))
		     (or (equal-formulas? current-statement
					  (left-disjunct original-statement))
			 (equal-formulas? current-statement
					  (right-disjunct original-statement))))
	     :failure-message "To defend against the WHICH-DISJUNCT? attack,~%assert either the left or the right disjunct~%of the original disjunction."))

(defvar rule-d02-implication
  (make-rule :name d02-implication
	     :condition (and (eq current-stance 'd)
			     (let* ((attack (nth-move dialogue current-reference))
				    (attack-refers-to (move-reference attack))
				    (original-move (nth-move dialogue attack-refers-to))
				    (original-statement (move-statement original-move)))
			       (implication? original-statement)))
	     :body (let* ((attack (nth-move dialogue current-reference))
			  (attack-refers-to (move-reference attack))
			  (original-move (nth-move dialogue attack-refers-to))
			  (original-statement (move-statement original-move)))
		     (equal-formulas? current-statement
				      (consequent original-statement)))
	     :failure-message "To defend against an attack on an implication, assert its consequent."))

(defvar rule-d02-negation
  (make-rule :name d02-negation
	     :condition (and (eq current-stance 'd)
			     (let* ((attack (nth-move dialogue current-reference))
				    (attack-refers-to (move-reference attack))
				    (original-move (nth-move dialogue attack-refers-to))
				    (original-statement (move-statement original-move)))
			       (negation? original-statement)))
	     :body t
	     :failure-message "One cannot (directly) defend against an attack on a negation."))

(defvar rule-d02-universal
  (make-rule :name d02-universal
	     :condition (and (eq current-stance 'd)
			     (let* ((attack (nth-move dialogue current-reference))
				    (attack-refers-to (move-reference attack))
				    (original-move (nth-move dialogue attack-refers-to))
				    (original-statement (move-statement original-move)))
			       (universal? original-statement)))
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
  (make-rule :name d02-existential
	     :condition (and (eq current-stance 'd)
			     (let* ((attack (nth-move dialogue current-reference))
				    (attack-refers-to (move-reference attack))
				    (original-move (nth-move dialogue attack-refers-to))
				    (original-statement (move-statement original-move)))
			       (existential? original-statement)))
	     :body (let* ((attack (nth-move dialogue current-reference))
			  (attack-refers-to (move-reference attack))
			  (original-move (nth-move dialogue attack-refers-to))
			  (original-statement (move-statement original-move)))
		     (instance-of-quantified? current-statement
					      original-statement))
	     :failure-message "The asserted statement is not an instance of the original existential generalization."))

(defvar rule-d10
  (make-rule :name d10
	     :condition (and (evenp current-position) 
			     (atomic-formula? current-statement))
	     :body (some-move #'(lambda (move)
				  (when (opponent-move? move)
				    (equal-formulas? (move-statement move)
						     current-statement)))
			      dialogue)
	     :failure-message "Proponent cannot assert an atomic formula before opponent has asserted it."))

(defun closed-attack-indices (dialogue)
  (let ((defenses (remove-if #'attacking-move? (dialogue-plays dialogue))))
    (mapcar #'move-reference defenses)))

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
  (make-rule :name d11
	     :condition (eq current-stance 'd)
	     :body (let ((most-recent (most-recent-open-attack dialogue)))
		     (or (null most-recent)
			 (= most-recent current-reference)))
	     :failure-message "You must defend against only the most recent open attack."))

(defvar rule-d12
  (make-rule :name d12
	     :condition (eq current-stance 'd)
	     :body (every-move #'(lambda (move)
				   (or (initial-move? move)
				       (attacking-move? move)
				       (/= (move-reference move)
					   current-reference)))
			       dialogue)
	     :failure-message "Attacks may be answered at most once."))

(defvar rule-d13
  (make-rule :name d13
	     :condition (and (oddp current-position)
			     (eq current-stance 'a))
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

(defvar argumentation-forms '(rule-d01-conjunction
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

(defvar d-dialogue-rules (append argumentation-forms '(rule-d00-atomic
						       rule-d00-proponent
						       rule-d00-opponent
						       rule-d01-composite
						       rule-d01-adheres-to-forms
						       rule-d02-attack
						       rule-d02-adheres-to-forms
						       rule-d10
						       rule-d11
						       rule-d12
						       rule-d13)))

(defvar e-dialogue-rules (append d-dialogue-rules '(rule-e)))
	
(defstruct (dialogue
	     (:print-function print-dialogue)
	     (:constructor make-dialogue-int))
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

(defvar symbolic-attacks
  '(attack-left-conjunct attack-right-conjunct which-instance? which-disjunct?))

(defun symbolic-attack? (obj)
  (member obj symbolic-attacks))

(defun statement? (obj)
  (or (formula? obj)
      (term? obj)
      (symbolic-attack? obj)))

(defun read-statement ()
  (let (response)
    (until (statement? response)
      (setf response (read t nil nil)))
    response))

(defun read-statement-or-symbols (&rest symbols)
  (let (response)
    (until (or (statement? response)
	       (member response symbols))
      (setf response (read t nil nil)))
    response))

(defun read-formula-or-term ()
  (let (response)
    (until (or (formula? response)
	       (term? response))
      (setf response (read t nil nil)))
    response))

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

(defun play-dialogue-game (rules)
  (let ((dialogue nil)
	(turn-number 0)
	(player nil)
	(stance nil)
	(index nil)
	(statement nil))
    (tagbody (go initial-move)
     initial-move
       (msg "Let's play a dialogue game!~%")
       (msg "Proponent starts by playing a composite formula.~%")
       (msg "Input a composite formula: ")
       (setf dialogue (make-dialogue (read-composite-formula)))
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
       (setf statement (read-formula))
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