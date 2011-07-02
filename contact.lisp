;;; contact.lisp

(in-package :dialogues)

(defcomponent contact-page ()
  ())

(defmethod render ((self contact-page))
  (let ((mailto-uri (format nil "mailto:~a" *maintainer-email*)))
    (<:h1 "Contact")
    (<:p "Questions? Bugs? Feature requests?  Comments?  You're welcome to email the site maintainer; use the address below." )
    (<:blockquote
     (<:address
      (<:a :href mailto-uri *maintainer-email*)))))

;;; contact.lisp