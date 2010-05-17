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

(defclass dialogue-application (standard-application cookie-session-application)
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
  '(("Peirce's formula" "peirce-formula" peirce-formula)
    ("Excluded middle" "excluded-middle" excluded-middle)
    ("Weak excuded middle" "weak-excluded-middle" weak-excluded-middle)
    ("Dummet's formula" "dummett-formula" dummett-formula)
    ("Double negation introduction" "double-negation-intro" double-negation-intro)
    ("Double negation elimination" "double-negation-elim" double-negation-elim)
    ("Markov's formula" "markov-formula" markov-formula)
    ("K formula" "k-formula" k-formula)
    ("B formula" "b-formula" b-formula)
    ("C formula" "c-formula" c-formula)
    ("W formula" "w-formulas" w-formula)
    ("Scott's formula" "scott-formula" scott-formula)
    ("Smetanich's formula" "smetanich-formula" smetanich-formula)))

(defclass game-manipulator ()
  ((game :accessor game
	 :initarg :game
	 :backtrack t)))

(defmethod/cc start-game ((self game-manipulator) initial-formula)

(defmethod render ((self initial-formula-window))
  (<:h1 "It's your turn")
  (<:form :method "POST"
	  :target "play"
    (<:p (<:as-html "Enter a formula in the text box ")
	 (<:input :type "text" :name "input-formula")
	 (<:as-html " or select a famous formula from the menu: ")
	 (<:select :name "selected-formula" :size 1
	    (dolist (famous-formula famous-formulas)
	      (destructuring-bind (long-name short-name formula)
		  famous-formula
		(declare (ignore formula))
		(<:option :id short-name (<:as-html long-name)))))
	 (<:input :type "submit"
		  :value "Let's play"))))

;;; Game presentation component

(defcomponent play-window-component
    ())

(defmethod render ((self play-window-component))

(defentry-point "play" (:application *dialogue-application*)
    ((initial-formula (make-dialogue)
  (call 'play-window-component))

;;; ucw-site.lisp ends here