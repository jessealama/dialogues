;;; ucw-site.lisp A UCW-based dialogue site

(in-package :dialogue-site)

;; Boring server configuration

(defun make-dialogue-backend ()
  (make-backend
   :httpd
   :host "localhost"
   :port 9090))

(defun make-dialogue-server ()
  (make-instance
   'standard-server
   :backend (make-dialogue-backend)))

(defvar *dialogue-server* (make-dialogue-server))

(defun startup-dialogue-server ()
  (startup-server *dialogue-server*))

(defun shutdown-dialogue-server ()
 (shutdown-server *dialogue-server*))

;;;; The definiton of the dialogue

(defclass dialogue-application (standard-application cookie-session-application-mixin)
  ()
  (:default-initargs
   :url-prefix "/"
    :debug-on-error t))

(defvar *dialogue-application* (make-instance 'dialogue-application))

(register-application *dialogue-server* *dialogue-application*)

(defentry-point "" (:application *dialogue-application*)
    ()
  (call 'initial-formula-window))

(defcomponent initial-formula-window (standard-window-component)
  ()
  (:default-initargs
      :title "the game is about to begin"
      :styesheet nil))

(defvar famous-formulas
  `(("Peirce's formula" "peirce-formula" ,peirce-formula)
    ("Excluded middle" "excluded-middle" ,excluded-middle)
    ("Weak excuded middle" "weak-excluded-middle" ,weak-excluded-middle)
    ("Dummet's formula" "dummett-formula" ,dummett-formula)
    ("Double negation introduction" "double-negation-intro" ,double-negation-intro)
    ("Double negation elimination" "double-negation-elim" ,double-negation-elimination)
    ("Markov's formula" "markov-formula" ,markov-formula)
    ("K formula" "k-formula" ,k-formula)
    ("B formula" "b-formula" ,b-formula)
    ("C formula" "c-formula" ,c-formula)
    ("W formula" "w-formulas" ,w-formula)
    ("Scott's formula" "scott-formula" ,scott-formula)
    ("Smetanich's formula" "smetanich-formula" ,smetanich-formula)))

(defclass game-manipulator ()
  ((game :accessor game
	 :initarg :game)))

(defaction start-game ((self game-manipulator) initial-formula)
  (let ((new-game (make-dialogue initial-formula pqrs-signature)))
    (setf (game game-manipulator) new-game))
  (answer))

(defcomponent game-manipulator-component (game-manipulator)
  ())

(defmethod render ((self game-manipulator-component))
  ()
  (<:p "Here's the game:" (<:br)
       (<:as-html (format nil "~A" (game self)))

(defmethod render ((self initial-formula-window))
  (let (input-formula selected-formula)
    (<:h1 "It's your turn")
    (<ucw:form :method "POST"
      (<:p (<:label :for "input-formula" "Enter a formula in the text box")
	   (<ucw:input :type "text" :accessor input-formula :id "input-formula")
	   (<:label :for "selected-formula" "or select a famous formula from the menu")
	   (<ucw:select :id "selected-formula" 
			:size 1 
			:accessor selected-formula
	     (dolist (famous-formula famous-formulas)
	       (destructuring-bind (long-name short-name formula)
		   famous-formula
		 (declare (ignore short-name))
		 (<ucw:option :value formula (<:as-html long-name))))))
      (<:p
       (<:as-html "If the text box is not empty, its contents will be the initial formula.  If the text box is empty, then the selected \"famous formula\" will be used."))
      (<:p
       (<ucw:submit :action (call 'game-manipulator-component 
				  :game (make-dialogue (if (empty-string? input-formula)
							   selected-formula
							   markov-formula)
						       pqrs-propositional-signature))
		    :value "Let's play")))))

;;; ucw-site.lisp ends here