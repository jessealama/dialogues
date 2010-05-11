
(in-package :cl-user)

(defpackage :dialogues-asd
  (:use :cl :asdf))

(in-package :dialogues-asd)

(defsystem :dialogues
  :description "A system for working with and exploring Lorenzen dialogue games"
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:hunchentoot)
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "formulas")
	       (:file "dialogues")
	       (:file "queue")
	       (:file "search")
	       (:file "dialogue-search")
	       (:file "figure")
	       (:file "felscher")
	       (:module site
			:serial t
			:components ((:file "packages")
				     (:file "site")))))
