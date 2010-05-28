
(in-package :dialogues)

(defclass dialogue-application (standard-application cookie-session-application-mixin)
  ()
  (:default-initargs
   :url-prefix "/"
    :debug-on-error nil))

(defvar *dialogue-application* (make-instance 'dialogue-application))

(register-application *dialogue-server* *dialogue-application*)

;;; ucw-applications.lisp ends here