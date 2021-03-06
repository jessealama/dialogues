(asdf:defsystem :dialogues
  :description "A system for working with and exploring Lorenzen dialogue games"
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t
  :depends-on (:alexandria
               :named-readtables
               :yacc
               :fiveam
               :cl-fad)
  :components ((:file "packages")
	       (:file "utils")
               (:file "queue")
               (:file "expressions")
               (:file "tptp")
               (:file "parse")
	       (:file "translations")
               (:file "move")
	       (:file "dialogue")
	       (:file "search")
	       (:file "strategy")
	       (:file "dialogue-search")))
