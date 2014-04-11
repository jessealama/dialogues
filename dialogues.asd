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
	       ;; (:file "signature")
	       (:file "terms")
	       (:file "formulas")
               (:file "tptp")
               (:file "parse")
	       (:file "symbolic-attack")
	       ;; (:file "enumerate")
	       (:file "translations")
	       (:file "famous-formulas")
	       (:file "dialogues")
	       (:file "queue")
	       (:file "search")
	       (:file "felscher")
	       (:file "strategy")
	       (:file "dialogue-search")
               ;; (:file "expressions")
               (:file "tptp")
               (:file "parse")))
