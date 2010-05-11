(defvar dependencies '(cl-who parenscript url-rewrite hunchentoot)

(dolist (d dependencies)
  (asdf:oos 'asdf:load-op d))

(defpackage :dialogue-site)
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :dialogue-site)
