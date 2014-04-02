
(in-package :dialogues)

;;; The lexer

(define-condition lexer-error (yacc-runtime-error)
  ((character :initarg :character :reader lexer-error-character))
  (:report (lambda (e stream)
             (format stream "Lexing failed~@[: unexpected character ~S~]"
                     (lexer-error-character e)))))

(defun lexer-error (char)
  (error (make-condition 'lexer-error :character char)))

(defun maybe-unread (char stream)
  (when char
    (unread-char char stream)))

(defun read-word (stream)
  (let ((v '()))
    (loop
       (let ((c (read-char stream nil nil)))
         (when (or (null c)
                   (not (or (digit-char-p c)
			    (alpha-char-p c)
			    (char-equal c #\_))))
           (maybe-unread c stream)
           (when (null v)
             (lexer-error c))
	   (return-from read-word (coerce (nreverse v) 'string)))
         (push c v)))))

(defun read-quoted-atom (stream)
  (loop
     :with v = nil
     :with first-single-quote-read = nil
     :for c = (read-char stream nil nil)
     :do
     (cond ((null c)
	    (lexer-error (if (null v)
			     "read-quoted-atom failed"
			     (car v))))
	   ((char= c #\')
	    (when first-single-quote-read
	      (return-from read-quoted-atom
		(coerce (nreverse v) 'string)))
	    (setf first-single-quote-read t))
	   (t
	    (push c v)))))

(defun read-integer (stream)
  (loop
     :with v = nil
     :for c = (read-char stream nil nil)
     :do
     (cond ((null c)
	    (lexer-error (if (null v)
			     "read-integer failed"
			     (car v))))
	   ((digit-char-p c)
	    (push c v))
	   (t
	    (unread-char c stream)
	    (return-from read-integer
	      (parse-integer (coerce (nreverse v) 'string)))))))

(defparameter *tptp-keywords*
  (list

   ;; annotated formula keywords
   "fof"
   "cnf"

   "include"

   ;; formula roles

	))

(defparameter *formula-roles*
  (list "axiom"
	"hypothesis"
	"definition"
	"assumption"
	"lemma"
	"theorem"
	"conjecture"
	"negated_conjecture"
	"plain"
	"fi_domain"
	"fi_functors"
	"fi_predicates"
	"type"
	"unknown"
	))

(defparameter *whitespace-characters*
  '(#\Space #\Tab #\Newline #\Return))

(let (toplevel-p num-commas-read within-include)

  (defun initialize-lexer ()
    (setf toplevel-p t)
    (setf num-commas-read 0)
    (setf within-include nil)
    t)

  (defun lexer (stream)
    (loop
       for c = (read-char stream nil nil)
       do
	 (cond

	   ((null c)
	    (return-from lexer (values nil nil)))

	   ((char= c #\%)
	    (read-line stream)) ;; consume comment lines

	   ((member c *whitespace-characters*)) ;; consume whitespace

	   ((digit-char-p c)
	    (unread-char c stream)
	    (return-from lexer (values (intern "integer" :dialogues)
				       (read-integer stream))))

	   ((char= c #\')
	    (unread-char c stream)
	    (let ((quoted (read-quoted-atom stream)))
	      (return-from lexer (values (intern "single-quoted" :dialogues) quoted))))

	   ((char= c #\$)
	    (return-from lexer (values (intern "$" :dialogues) "$")))

	   ((member c '(#\( #\) #\. #\[ #\] #\: #\! #\? #\, #\< #\~ #\= #\&))
	    ;; (break "Got a symbol: ~a" c)
	    (when (char= c #\.)
	      (initialize-lexer))

	    (when (char= c #\,)
	     (incf num-commas-read)
	     (return-from lexer (values (intern "," :dialogues) ",")))

	    (when (char= c #\~)
	      (let ((after-~ (read-char stream nil nil)))
		(cond ((null after-~)
		       (lexer-error #\~))
		      ((member after-~ *whitespace-characters*)
		       (unread-char after-~ stream)
		       (return-from lexer (values (intern "~" :dialogues) "~")))
		      ((char= after-~ #\&)
		       (return-from lexer (values (intern "~&" :dialogues) "~&")))
		      (t
		       (unread-char after-~ stream)
		       (return-from lexer (values (intern "~" :dialogues) "~"))))))

	    (when (char= c #\!)
	      (let ((after-! (read-char stream nil nil)))
		(cond ((null after-!)
		       (lexer-error #\!))
		      ((member after-! *whitespace-characters*)
		       (unread-char after-! stream)
		       (return-from lexer (values (intern "!" :dialogues) "!")))
		      ((char= after-! #\=)
		       (return-from lexer (values (intern "!=" :dialogues) "!=")))
		      (t
		       (unread-char after-! stream)
		       (return-from lexer (values (intern "!" :dialogues) "!"))))))

	    (when (char= c #\<)
	      (let ((after-< (read-char stream nil nil)))
		(cond ((null after-<)
		       (lexer-error #\<))
		      ((member after-< *whitespace-characters*)
		       (lexer-error after-<))
		      ((char= after-< #\=)
		       (let ((after-after-< (read-char stream nil nil)))
			 (cond ((null after-after-<)
				(lexer-error after-<))
			       ((char= after-after-< #\>)
				(return-from lexer (values (intern "<=>" :dialogues) "<=>")))
			       (t
				(return-from lexer (values (intern "<=" :dialogues) "<="))))))
		      ((char= after-< #\~)
		       (let ((after-after-< (read-char stream nil nil)))
			 (cond ((null after-after-<)
				(lexer-error after-<))
			       ((char= after-after-< #\>)
				(return-from lexer (values (intern "<~>" :dialogues) "<~>")))
			       (t
                                (lexer-error after-<)))))
		      (t
		       (lexer-error #\<)))))

	    (when (char= c #\=)
	      (let ((d (read-char stream nil nil)))
		(if d
		    (if (char= d #\>)
			(return-from lexer (values (intern "=>" :dialogues) "=>"))
			(progn
			    (unread-char d stream)
			    (return-from lexer (values (intern "=" :dialogues) "="))))
		    (lexer-error d))))

	    (return-from lexer (values (intern (string c) :dialogues) (string c))))

	   ((char= c #\|)
	    (return-from lexer (values (intern "|" :dialogues) "|")))

	   ;; try to read an atom

	   (toplevel-p
	    (unread-char c stream)
	    (let ((next-word (read-word stream)))
	      (if (member next-word *tptp-keywords* :test #'string=)
		  (progn
		    (when (string= next-word "include")
		      (setf within-include t))
		    (setf toplevel-p nil)
		    (return-from lexer (values (intern next-word :dialogues) next-word)))
		  (error "Don't know how to handle the toplevel word '~a'." next-word))))

	   ((and (= num-commas-read 1)
		 (not within-include))
	    (unread-char c stream)
	    (let ((next-word (read-word stream)))
	      (if (member next-word *formula-roles* :test #'string=)
		  (progn
		    (return-from lexer (values (intern next-word :dialogues) next-word)))
		  (error "Unknown formula role '~a'." next-word))))

	   ((alpha-char-p c)
	    (unread-char c stream)
	    (let ((next-word (read-word stream)))
	      ;; (break "next-word = ~a" next-word)
	      (cond ((lower-case-p c)
		     (return-from lexer (values (intern "lower-word" :dialogues) next-word)))
		    ((upper-case-p c)
		     (return-from lexer (values (intern "upper-word" :dialogues) next-word)))
		    (t
		     (error "Don't know how to handle '~a'." next-word)))))

	   (t
	    (lexer-error c))))))

;;; The parser and semantic actions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun k-2-3 (a b c)
    "Second out of three."
    (declare (ignore a c))
    b)
  )

(define-parser *tptp-v5.4.0.0-parser*
  (:start-symbol tptp-file)
  (:terminals (|fof|
	       |cnf|
	       |(|
	       |)|
	       |.|
	       |,|
	       |[|
	       |]|
	       |=|
	       |lower-word|
	       |upper-word|
	       |$|
	       |'|
	       |axiom|
	       |hypothesis|
	       |definition|
	       |assumption|
	       |lemma|
	       |theorem|
	       |conjecture|
	       |negated_conjecture|
	       |plain|
	       |fi_domain|
	       |fi_functors|
	       |fi_predicates|
	       |type|
	       |unknown|
	       |single-quoted|
	       |include|
	       |integer|
	       |<=>|
	       |=>|
	       |<=|
	       |<~>|
	       |~&|
	       |:|
	       |!|
	       |?|
	       |~|
	       |!=|
	       |&|
	       |\||
	       |axiom_of_choice|
	       |tautology|
	       ))

  (tptp-file
   tptp-inputs)

  (tptp-inputs
   ()
   (tptp-input tptp-inputs
	       #'(lambda (arg-1 arg-2)
		   (make-instance 'tptp-db
				  :formulas (cons arg-1
						  (when arg-2
						    (formulas arg-2)))))))

  (tptp-input
   annotated-formula
   include)

  (include
   (|include| |(| file-name formula-selection |)| |.|
	      #'(lambda (word left-paren file-name formula-selection right-paren full-stop)
		     (declare (ignore left-paren right-paren full-stop))
		     (unless (string= word "include")
		       (error "We are expecting 'include', but we received '~a'." word))
		     (make-instance 'include-instruction
				    :file file-name
				    :selection formula-selection)))
   )

  (file-name
   (|single-quoted| #'(lambda (x)
			(format nil "'~a'" x))))

  (formula-selection
   ()
   (|,| |[| name-list |]|
	#'(lambda (comma left-bracket stuff right-bracket)
	    (declare (ignore comma left-bracket right-bracket))
	    stuff)))

  (name-list
   (name #'list)
   (name |,| name-list
	 #'(lambda (head comma tail)
	     (declare (ignore comma))
	     (cons head tail))))

  (name
   atomic-word
   integer)

  (annotated-formula
   fof-annotated
   cnf-annotated)

  (fof-annotated
   (|fof| |(| name |,| formula-role |,| fof-formula |)| |.|
	  #'(lambda (fof-symbol left-paren name comma-1 role comma-2 formula right-paren full-stop)
	      (declare (ignore fof-symbol
			       left-paren
			       comma-1
			       comma-2
			       right-paren
			       full-stop))
	      (make-instance 'fof
			     :name name
			     :role role
			     :formula formula)))
   (|fof| |(| name |,| formula-role |,| fof-formula |,| source |)| |.|
	  #'(lambda (fof-symbol left-paren name comma-1 role comma-2 formula comma-3 source right-paren full-stop)
	      (declare (ignore fof-symbol
			       left-paren
			       comma-1
			       comma-2
			       comma-3
			       right-paren
			       full-stop))
	      (make-instance 'fof
			     :name name
			     :role role
			     :formula formula
			     :source source)))
   (|fof| |(| name |,| formula-role |,| fof-formula |,| source |,| optional-info|)| |.|
	  #'(lambda (fof-symbol left-paren name comma-1 role comma-2 formula comma-3 source comma-4 optional-info right-paren full-stop)
	      (declare (ignore fof-symbol
			       left-paren
			       comma-1
			       comma-2
			       comma-3
			       comma-4
			       right-paren
			       full-stop))
	      (make-instance 'fof
			     :name name
			     :role role
			     :formula formula
			     :source source
			     :optional-info optional-info))))

  (cnf-annotated
   (|cnf| |(| name |,| formula-role |,| cnf-formula |)| |.|
	  #'(lambda (cnf-symbol left-paren name comma-1 role comma-2 formula right-paren full-stop)
	      (declare (ignore cnf-symbol
			       left-paren
			       comma-1
			       comma-2
			       right-paren
			       full-stop))
	      (make-instance 'cnf
			     :name name
			     :role role
			     :formula formula)))
   (|cnf| |(| name |,| formula-role |,| cnf-formula |,| source |)| |.|
	  #'(lambda (cnf-symbol left-paren name comma-1 role comma-2 formula comma-3 source right-paren full-stop)
	      (declare (ignore cnf-symbol
			       left-paren
			       comma-1
			       comma-2
			       comma-3
			       right-paren
			       full-stop))
	      (make-instance 'cnf
			     :name name
			     :role role
			     :formula formula
			     :source source)))
      (|cnf| |(| name |,| formula-role |,| cnf-formula |,| source |,| optional-info |)| |.|
	     #'(lambda (cnf-symbol left-paren name comma-1 role comma-2 formula comma-3 source comma-4 optional-info right-paren full-stop)
		 (declare (ignore cnf-symbol
				  left-paren
				  comma-1
				  comma-2
				  comma-3
				  comma-4
				  right-paren
				  full-stop))
		 (make-instance 'cnf
				:name name
				:role role
				:formula formula
				:source source
				:optional-info optional-info))))

  (optional-info
   useful-info
   ())

  (source
   (general-term
    #'(lambda (x)
	(if (typep x 'atomic-expression)
	    (with-slots (head arguments)
		x
	      (if (and (string= (stringify head) "inference")
		       (length= 3 arguments)
		       (typep (third arguments) 'general-list))
		  (make-instance 'inference-record
				 :rule (first arguments)
				 :useful-info (second arguments)
				 :parents (third arguments))
		  x))
	    x))))

  (sources
   source
   (source |,| sources))

  (internal-source
   (|introduced| |(| intro-type optional-info |)|))

  (intro-type
   |definition|
   |axiom_of_choice|
   |tautology|
   |assumption|)

  (external-source
   file-source
   theory
   creator-source)

  (file-source
   ;; (|file| |(| file-name file-info |)|)
   (|lower-word| |(| file-name file-info |)|)) ;; the content of the
					       ;; lower-word should be
					       ;; the string "file"

  (file-info
   (|,| name)
   ()
   )

  (dag-source
   name
   inference-record)

  (inference-record
   ;; the content of the first lower-word should be "inference"
   (|lower-word| |(| inference-rule |,| useful-info |,| |[| parent-list |]| |)|)


   ;; (|inference| |(| inference-rule |,| useful-info |,| |[| parent-list |]|)
   )


  (parent-list
   parent-info
   (parent-info |,| parent-list))

  (parent-info
   (source parent-details))

  (parent-details
   (|:| general-list)
   ())

  (inference-rule
   atomic-word)

  (useful-info
   general-list)

  (general-list
   (|[| |]|
	#'(lambda (left-bracket right-bracket)
	    (declare (ignore left-bracket right-bracket))
	    (make-instance 'general-list
			   :terms nil)))
   (|[| general-terms |]|
	#'(lambda (left-bracket stuff right-bracket)
	    (declare (ignore left-bracket right-bracket))
	    (make-instance 'general-list
			   :terms stuff))))

  (general-terms
   (general-term #'(lambda (x) (list x)))
   (general-term |,| general-terms
		 #'(lambda (head comma tail)
		     (declare (ignore comma))
		     (cons head tail))))

  (general-term
   general-data
   (general-data |:| general-term)
   general-list)

  (general-data
   atomic-word
   general-function
   variable
   number
   ;; distinct-object  (not yet supported)
   ;; formula-data  (not yet supported)
   )

  (general-function
   (atomic-word |(| general-terms |)|
		#'(lambda (word left-paren terms right-paren)
		    (declare (ignore left-paren right-paren))
		    (make-instance 'atomic-expression
				   :head (intern word :dialogues)
				   :arguments terms))))

  (name
   atomic-word
   |integer|)

  (number
   |integer|
   ;; rational  (not yet supported)
   ;; real  (not yet supported)
   )

  (atomic-word
   |lower-word|
   |single-quoted|)

  (formula-role
   |axiom|
   |hypothesis|
   |definition|
   |assumption|
   |lemma|
   |theorem|
   |conjecture|
   |negated_conjecture|
   |plain|
   |fi_domain|
   |fi_functors|
   |fi_predicates|
   |type|
   |unknown|)

  (fof-formula
   fof-logic-formula
   fof-sequent)

  (cnf-formula
   (|(| disjunction |)|
	#'(lambda (left-paren x right-paren)
	    (declare (ignore left-paren right-paren))
	    x))
   disjunction)

  (disjunction
   literal
   (literal |\|| disjunction
	    #'(lambda (head vbar tail)
		(declare (ignore vbar))
		(make-instance 'binary-disjunction
			       :lhs head
			       :rhs tail))))

  (literal
   atomic-formula
   (|~| atomic-formula
	#'(lambda (neg formula)
	    (declare (ignore neg))
	    (if (typep formula 'equation)
		(make-instance 'disequation
			       :lhs (lhs formula)
			       :rhs (rhs formula))
		(make-instance 'negation
			       :argument formula))))
   fol-infix-unary)

  (fof-logic-formula
   fof-binary-formula
   fof-unitary-formula)

  (fof-binary-formula
   fof-binary-nonassoc
   fof-binary-assoc)

  (fof-binary-nonassoc
   (fof-unitary-formula fof-binary-connective fof-unitary-formula
			#'(lambda (lhs connective rhs)
			    (make-instance (cond ((string= connective "<=>")
						  'equivalence)
						 ((string= connective "<~>")
						  'nonequivalence)
						 ((string= connective "=>")
						  'implication)
						 ((string= connective "<=")
						  'reverse-implication))
					   :lhs lhs
					   :rhs rhs))))

  (fof-binary-assoc
   fof-or-formula
   fof-and-formula)

  (fof-or-formula
   (fof-unitary-formula |\|| fof-unitary-formula
			#'(lambda (disjunct-1 vbar disjunct-2)
			    (declare (ignore vbar))
			    (make-instance 'binary-disjunction
					   :lhs disjunct-1
					   :rhs disjunct-2)))
   (fof-or-formula |\|| fof-unitary-formula
		   #'(lambda (disjunction vbar disjunct)
		       (declare (ignore vbar))
		       (make-instance 'binary-disjunction
				      :lhs disjunction
				      :rhs disjunct))))

  (fof-and-formula
   (fof-unitary-formula |&| fof-unitary-formula
			#'(lambda (conjunct-1 ampersand conjunct-2)
			    (declare (ignore ampersand))
			    (make-instance 'binary-conjunction
					   :lhs conjunct-1
					   :rhs conjunct-2)))
   (fof-and-formula |&| fof-unitary-formula
		    #'(lambda (conjunction ampersand conjunct)
			    (declare (ignore ampersand))
			    (make-instance 'binary-conjunction
					   :lhs conjunction
					   :rhs conjunct))))

  (fof-binary-connective
   |<=>|
   |=>|
   |<=|
   |<~>|
   ;; |~\||
   |~&|)

  (fof-unitary-formula
   (|(| fof-logic-formula |)|
	#'(lambda (left-paren formula right-paren)
	    (declare (ignore left-paren right-paren))
	    formula))
   fof-quantified-formula
   fof-unary-formula
   atomic-formula)

  (fof-quantified-formula
   (fol-quantifier |[| fof-variable-list |]| |:| fof-unitary-formula
		   #'(lambda (quantifier left-bracket variable-list right-bracket colon matrix)
		       (declare (ignore left-bracket right-bracket colon))
		       (make-instance (cond ((string= quantifier "!")
					     'universal-generalization)
					    ((string= quantifier "?")
					     'existential-generalization)
					    (t
					     (error "Unknown quantifier '~a'." quantifier)))
				      :bindings variable-list
				      :matrix matrix))))

  (atomic-formula
   plain-atomic-formula
   defined-atomic-formula
   ;; system-atomic-formula
   )

  (plain-atomic-formula
   (plain-term #'(lambda (x)
		   (let ((head (head x))
			 (args (arguments x)))
		     (if (string= (format nil "~a" head) "=")
			 (make-instance 'equation
					:lhs (first args)
					:rhs (second args))
			 (make-instance 'atomic-formula
				    :predicate (head x)
				    :arguments (arguments x)))))))

  (defined-atomic-formula
      defined-plain-formula
      defined-infix-formula)

  (defined-infix-formula
      (term defined-infix-pred term
	    #'(lambda (left pred right)
		(unless (string= (format nil "~a" pred) "=")
		  (error "Unknown infix predicate '~a'." pred))
		(make-instance 'equation
			       :predicate (intern "=" :dialogues)
			       :arguments (list left right)
			       :lhs left
			       :rhs right))))

  (defined-infix-pred
      infix-equality)

  (infix-equality
   (|=| #'(lambda (x)
	    (declare (ignore x))
	    (intern "=" :dialogues))))

  (defined-plain-formula
      defined-prop
      (defined-pred |(| arguments |)|))

  (defined-prop
      ;; should be only "$true" and "$false"
      (atomic-defined-word #'(lambda (word)
			       (cond ((string= word "true") *nullary-true*)
				     ((string= word "false") *nullary-false*)
				     (t
				      (error "Unknown atomic defined word '~a'." word))))))

  (atomic-defined-word
   dollar-word)

  (dollar-word
   (|$| |lower-word|
	#'(lambda (dollar word)
	    (declare (ignore dollar))
	    word)))

  (fol-quantifier
   |!|
   |?|)

  (fof-variable-list
   (variable
    #'(lambda (x) (list x)))
   (variable |,| fof-variable-list
	     #'(lambda (head-variable comma more-variables)
		 (declare (ignore comma))
		 (cons head-variable more-variables))))

  (variable
   (|upper-word| #'(lambda (x)
		     (make-instance 'variable-term
				    :head (intern x :dialogues)))))

  (fof-unary-formula
   (unary-connective fof-unitary-formula
		     #'(lambda (connective argument)
			 (unless (string= connective "~")
			   (error "Unknown unary connective '~a'." connective))
			 (if (typep argument 'equation)
			     (make-instance 'disequation
					    :lhs (lhs argument)
					    :rhs (rhs argument))
			     (make-instance 'negation
					    :argument argument))))
   fol-infix-unary)

  (unary-connective
   |~|)

  (fol-infix-unary
   (term infix-inequality term
	 #'(lambda (lhs disequality-symbol rhs)
	     (declare (ignore disequality-symbol))
	     (make-instance 'disequation
			    :lhs lhs
			    :rhs rhs))))

  (term
   function-term
   variable
   ;; conditional-term
   ;; let-term
   )

  (function-term
   (plain-term #'(lambda (x)
		   (make-instance 'function-term
				  :head (head x)
				  :arguments (arguments x))))
   defined-term
   system-term)

  (plain-term
   (constant
    #'(lambda (c)
	(make-instance 'atomic-expression
		       :head (intern c :dialogues)
		       :arguments nil)))
   (functor |(| arguments |)|
	    #'(lambda (f lparen args rparen)
		(declare (ignore lparen rparen))
		(make-instance 'function-term
			       :head (intern f :dialogues)
			       :arguments args))))

  (constant
   functor)

  (functor
   atomic-word)

  (arguments
   (term #'(lambda (x)
	     (list x)))
   (term |,| arguments
	 #'(lambda (head-term comma more-terms)
	     (declare (ignore comma))
	     (cons head-term more-terms))))

  (defined-term
      defined-atom
      defined-atomic-term)

  (defined-atom
      ;; number
      ;; distinct-object
      )

  (defined-atomic-term
      defined-plain-term)

  (defined-plain-term
      defined-constant
      (defined-functor |(| arguments |)|))

  (defined-constant
      defined-functor
      defined-type)

  (infix-inequality
   |!=|)

)


;;; The evaluator

(define-condition evaluator-error (yacc-runtime-error)
  ((expression :initarg :expression :reader evaluator-error-expression))
  (:report (lambda (e stream)
             (format stream "Couldn't evaluate expression ~S"
                     (evaluator-error-expression e)))))

;;; The toplevel loop

(defgeneric parse-tptp (tptp))

(defmethod parse-tptp ((tptp-string string))
  (with-input-from-string (string tptp-string)
    (initialize-lexer)
    (parse-with-lexer #'(lambda ()
			  (lexer string))
		      *tptp-v5.4.0.0-parser*)))

(defmethod parse-tptp ((tptp-path pathname))
  (with-open-file (tptp-stream tptp-path
			       :direction :input
			       :if-does-not-exist :error)
    (initialize-lexer)
    (let ((db (parse-with-lexer #'(lambda ()
				    (lexer tptp-stream))
				*tptp-v5.4.0.0-parser*)))
      (setf (path db) tptp-path)
      db)))

(defmethod parse-tptp ((tptp tptp-db))
  tptp)

(defun lex-tptp-formula (string)
  (with-input-from-string (stream string)
    (loop
       initially (initialize-lexer)
       for token = (lexer stream)
       collect token into tokens
       unless token return tokens)))

;;; parse.lisp ends here
