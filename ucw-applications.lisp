
(in-package :dialogues)

(defclass dialogue-application (standard-application cookie-session-application-mixin)
  ()
  (:default-initargs
   :url-prefix "/"
    :debug-on-error t))

(defvar *dialogue-application* (make-instance 'dialogue-application))

(register-application *dialogue-server* *dialogue-application*)

(defmethod handle-toplevel-condition ((app (eql *dialogue-application*))
				      condition
				      action)
  (let ((mailto-uri (format nil "mailto:~a" *maintainer-email*)))
    (<:h1 "Oops, something went wrong")
    (<:p "Something went wrong, I'm afraid.  Here is the precise error that was generated:")
    (<:blockquote
     (<:as-html condition))
    (<:p "Please" (<:a :href mailto-uri "notify the site maintainer") " about this.")
  (<:p "What next?  You can either go back with your browser or simply " 
       (<ucw:a :action (call 'start-game-component)
	       "quit and start over") ".")))

;;; ucw-applications.lisp ends here