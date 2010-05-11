;;; site.lisp A website for playing Lorenzen dialogue games

(in-package :dialogue-site)

;; Logging

(setq *message-log-pathname* "/tmp/dialogue-messages")
(setq *access-log-pathname* "/tmp/dialogue-access")

;; Dispatching

(defvar dialogue-dispatch-table
  (list (create-static-page-dispatcher "/" 'start-page)))

(defun dialogue-request-dispatcher (request)
  "Selects a request handler based on a list of individual request
dispatchers all of which can either return a handler or neglect by
returning NIL."
  (loop for dispatcher in dialogue-dispatch-table
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(define-xhtml-handler start-page ()
(defvar named-formulas
  `(("Peirce's formula" "peirce-formula" peirce-formula)
    ("Excluded middle" "excluded-middlge" excluded-middle)
    ("Weak excluded middle" "weak-excluded-middle" weak-excluded-middle)
    ("I-formula" "i-formula" i-formula)
    ("K-formula" "k-formula" k-formula)
    ("S-formula" "s-formula" s-formula)))

  (with-title "Let's play a dialogue game"
    (:p "Like this one:")))

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