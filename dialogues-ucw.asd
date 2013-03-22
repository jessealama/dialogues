(asdf:defsystem :dialogues-ucw
  :description "A system for working with and exploring Lorenzen dialogue games"
  :long-description "This system is intended to offer a web site where
one can explore logic through Lorenzen dialogue games.  (It may also
grow to not simply be a \"web site\" that it dishes out only (X)HTML
representations; it might come to offer all sorts of services.
The (X)HTML representation of the site might even come to be only one
of several aspects. It is based on the UnCommon Web system."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:ucw :dialogues)
  :components ((:module "ucw"
			:serial t
			:components ((:file "packages")
				     (:file "ucw-formulas")
				     (:file "ucw-utils")
				     (:file "ucw-server")
				     (:file "ucw-applications")
				     (:file "ucw-ruleset")
				     (:file "ucw-strategies")
				     (:file "about")
				     (:file "ucw-turns")
				     (:file "ucw-site")
				     (:file "start-ucw")))))
