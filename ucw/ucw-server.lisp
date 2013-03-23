
(in-package :dialogues-ucw)

;; Server configuration

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dialogue-server-port* 8000))

(defun make-dialogue-backend ()
  (make-backend
   :httpd
   :host "127.0.0.1"
   :port *dialogue-server-port*))

(defun make-dialogue-server ()
  (make-instance
   'standard-server
   :backend (make-dialogue-backend)))

(defvar *dialogue-server* (make-dialogue-server))

(defun startup-dialogue-server ()
  (startup-server *dialogue-server*))

(defun shutdown-dialogue-server ()
 (shutdown-server *dialogue-server*))

;;; ucw-server.lisp ends here