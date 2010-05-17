(in-package #:dialogue-site)

(defun make-dialogue-backend ()
  (make-backend :httpd :host "localhost" :port 8080))

(defclass dialogue-server (standard-server)
  ())

(defun make-dialogue-server ()
  (make-instance 'dialogue-server :backend (make-dialogue-backend)))

(defvar *dialogue-ucw-server* (make-dialogue-server))

(defclass dialogue-application (basic-application
				cookie-session-application-mixin)
  ()
  (:default-initargs
   :url-prefix "/"))

(defparameter *dialogue-ucw-application*
  (make-instance 'dialogue-application))

(register-application *dialogue-ucw-server* *dialogue-ucw-application*)

(defun startup-dialogues ()
  (startup-server *dialogue-ucw-server*))

(defun shutdown-dialogues ()
  (shutdown-server *dialogue-ucw-server*))

(defcomponent initial-component-window (standard-window-component)
  ()
  (:default-initargs
      :title "Enter an initial formula"
    :body (make-instance 'initial-formula-chooser-component)))

(defcomponent initial-formula-chooser-component ()
  ())

(defmethod render :around ((form initial-formula-chooser-component))
  (<:form :method "post" :action "play"
    (call-next-method)))

(defmethod render :before ((form initial-formula-chooser-component))
  (<:h1 (<:as-html "It's your turn"))
  (<:p (<:as-html "")))

(defmethod render ((form initial-formula-chooser-component))
  (<:as-html "Enter a formula: ") (<:text :name "input-formula")
  (<:as-html " or choose a 'famous' formula from the menu: ")
  (render (make-instance 'formula-dropdown-menu)))

(defmethod render :after ((form initial-formula-chooser-component))
  (<:br)
  (<:submit :value "Let's go"))

(defcomponent formula-dropdown-menu ()
  ())

(defvar named-formulas
  `(("Peirce's formula" "peirce-formula" peirce-formula)
    ("Excluded middle" "excluded-middlge" excluded-middle)
    ("Weak excluded middle" "weak-excluded-middle" weak-excluded-middle)
    ("I-formula" "i-formula" i-formula)
    ("K-formula" "k-formula" k-formula)
    ("S-formula" "s-formula" s-formula)))

(defmethod render ((formulas formula-dropdown-menu))
  (<:select :name "selected-formula"
    (dolist (named-formula named-formulas)
      (destructuring-bind (full-name short-name formula)
	  named-formula
	(declare (ignore formula))
	(<:option :value short-name (<:as-html full-name))))))

(defentry-point "dialogues" (:application *dialogue-ucw-application*
					  :with-call/cc nil)
    ()
  (render (make-instance 'initial-component-window)))
