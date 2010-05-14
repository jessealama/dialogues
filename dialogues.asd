
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
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "formulas")
	       (:file "dialogues")
	       (:file "queue")
	       (:file "search")
	       (:file "dialogue-search")
	       (:file "figure")
	       (:file "felscher")))

(defsystem :dialogue-site-ucw
  :description "A dynamic website for playing dialogue games"
  :long-description "This system is intended to offer a web site where
one can explore logic through Lorenzen dialogue games.  (It may also
grow to not simply be a \"web site\" that it dishes out only (X)HTML
representations; it might come to offer all sorts of services.
The (X)HTML representation of the site might even come to be only one
of several aspects.

It is based on the UnCommon Web system.  This system allows us to
define a number of high-level interactions.  It is intended as a first
attempt at using high-evel frameworks.  This is my first attempt at
making a non-trivial system with UCW.  I am wary that the cost of
taking on board all of its functionality might be too high.  I worry,
in particular, that the UCW-based site that I intend to build might
not be scalable, and that it will not offer all the features that I
need.  At the same time, these fears are--for the moment--behind held
in check because I simply don't know of any alternative framework that
could get the job done as well as UCW.  I hold it lighly and accept it
provisionally.

Another system with the same aims, based on the hunchentoot web
server, is defined by DIALOGUE-SITE-HUNCHENTOOT.  It takes into its
own hands many of the problems that are solved for us automaticaly by
UCW."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t ;; for the sake of simplicity
  :depends-on (:dialogues :ucw)
  :pathname "site"
  :components ((:file "packages")
	       (:file "ucw-site")))

(defsystem :dialogue-site-hunchentoot
  :description "A dynamic website for playing dialogue games"
  :long-description "This system is intended to be a pace where one
can explore logic through Lorenzen dialogue games.

It is based on the hunchentoot web server.  By using this system are
responsible for more of the site's functionality than with other
high-level site frameworks, such as UnCommon Web or weblocks.  But
with more responsibility comes more freedom, and greater control.  It
is not clear to me, as I write this site, that we really need all the
high-level features of, say, UCW, nor is it clear to me that
high-level frameworks can deliver to us what we want.

Another system with the same aims, based on UCW, is defined by
DIALOGUE-SITE-UCW."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :serial t ;; for the sake of simplicity
  :pathname "site"
  :components ((:file "packages")
	       (:file "hunchentoot-site")))
