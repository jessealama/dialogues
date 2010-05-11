
(in-package :cl-user)

(defpackage :dialogues-asd
  (:use :cl :asdf))

(in-package :dialogues-asd)

(defsystem :cl-who-kryukov
  :version "0.11.1-kryukov"
  :serial t
  :components ((:file "site/cl-who/packages")
               (:file "site/cl-who/specials")
               (:file "site/cl-who/who")))

(defsystem :dialogues
  :description "A system for working with and exploring Lorenzen dialogue games"
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:hunchentoot :cl-who-kryukov)
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
