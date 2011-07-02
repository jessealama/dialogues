;;; contact.lisp

(in-package :dialogues)

(defcomponent contact-page ()
  ())

(defmethod render ((self contact-page))
  (<:h1 "Contact")
  (<:p "Questions? Bugs? Feature requests?  Comments?  You're welcome to email the site maintainer; use the address below." )
  (<:blockquote
   (<:address
    (<:a :href "mailto:jesse.alama@gmail.com"
	 "jesse.alama@gmail.com"))))

;;; contact.lisp