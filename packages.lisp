
(in-package :cl-user)

(defpackage :dialogues
  (:use :cl :alexandria :yacc :named-readtables)
  (:import-from :cl-fad
		#:file-exists-p
		#:directory-exists-p
		#:directory-pathname-p
		#:pathname-as-directory))

(defpackage :dialogues-ucw
  (:use :cl :ucw :ucw-core :cl-ppcre :alexandria :dialogues))

(defvar *dialogue-package* (find-package :dialogues))

;;; packages.lisp ends here
