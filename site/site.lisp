;;; site.lisp A website for playing Lorenzen dialogue games

(in-package :dialogue-site)

;; Logging

(setq *message-log-pathname* "/tmp/dialogue-messages")
(setq *access-log-pathname* "/tmp/dialogue-access")

;; Dispatching

(defvar dialogue-dispatch-table
  (list (create-static-page-dispatcher "/" 'start-page)
	(create-static-page-dispatcher "/play" 'play-page)))

(defun dialogue-request-dispatcher (request)
  "Selects a request handler based on a list of individual request
dispatchers all of which can either return a handler or neglect by
returning NIL."
  (loop for dispatcher in dialogue-dispatch-table
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(defvar named-formulas
  `(("Peirce's formula" "peirce-formula" peirce-formula)
    ("Excluded middle" "excluded-middlge" excluded-middle)
    ("Weak excluded middle" "weak-excluded-middle" weak-excluded-middle)
    ("I-formula" "i-formula" i-formula)
    ("K-formula" "k-formula" k-formula)
    ("S-formula" "s-formula" s-formula)))

(define-xhtml-handler start-page ()
  (with-title "Let's play a dialogue game"
    (:h1 "It's your move, Proponent.")
    (:p "To get started, enter a formula in the text box (" (:a :href "format-for-formulas" "learn about the required format for formulas") ") or choose one of formulas from the menu (" (:a :href "some-famous-formulas" "learn about the formulas in this list") ").")
    (:form :action "play"
	   :enctype "multipart/form-data"
	   :method "post"
     (:input :type "text"
	     :name "input-formula")
     (:select :type "radio"
	      :name "famous-formula"
	      :size 1
       (dolist (named-formula named-formulas)
	 (destructuring-bind (name short-name formula)
	     named-formula
	   (declare (ignore formula))
	   (htm (:option :value name 
			 :name short-name
			 (str name))))))
     (:input :type "submit"
	     :value "Start"))))

(define-xhtml-handler play-page ()
  (multiple-value-bind (famous-formula input-formula)
      (fetch-post-parameters "famous-formula" "input-formula")
    (cond ((and (not (empty-string? famous-formula))
		(not (empty-string? input-formula)))
	   (with-title "Improper input"
	     (:p "I'm not sure how to interpret your intention for starting the game: you selected a \"named\" formula with which to start the game (namely, " (str famous-formula) "), but you also entered another formula in the text box (namely, " (str input-formula) ").")
    	     (:p "Please return to " (:a :href "/" "the start page") " and try again.")))
    	  ((and (empty-string? famous-formula)
    		(empty-string? input-formula))
    	   (with-title "Improper input"
    	     (:p "I'm not sure how to interpret your intention for starting the game: you selected neither one of the \"named\" formulas, nor did you enter your own formula.")
    	     (:p "Please return to " (:a :href "/" "the start page") " and try again.")))
    	  (t 
    	   (with-title "Play"
    	     (:p "OK, you gave me sensible input.  Let's roll."))))))

(defvar current-acceptor nil
  "The current hunchentoot acceptor object that's handling our requests.

\(For now, we have only one.  We give a value to this variable upon
startup and clear its value on shutdown.  Otherwise, we don't do
anything with it.)")

(defun startup (&optional (port 8080))
  (handler-case (progn
		  (setf current-acceptor 
			(make-instance 'acceptor 
				       :port port
				       :request-dispatcher 'dialogue-request-dispatcher))
		  (values t (start current-acceptor)))
    (usocket:address-in-use-error ()
      (values nil (format nil "Port ~A is already taken" port)))))

(defun shutdown ()
  (stop current-acceptor)
  (setf current-acceptor nil))

;;; site.lisp ends here