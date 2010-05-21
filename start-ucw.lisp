;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start-hunchentoot.lisp
;;;;
;;;; Author:  William Bruschi
;;;; Date:    02-14-2009
;;;;
;;;; Starts Hunchentoot and Swank, then listens for a shutdown
;;;; command on the specified port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'asdf)
(require 'sb-bsd-sockets)

(push "/home/jesse/src/dialogues" asdf:*central-registry*)
(asdf:oos 'asdf:load-op "dialogues")

(in-package :dialogues)

(defparameter *hunchentoot-port* 8000)
(defparameter *shutdown-port* 6440)
(defparameter *swank-loader*
"/PATH-TO-SLIME/slime/swank-loader.lisp")
(defparameter *swank-port* 4006)

;;; Start the hunchentoot server
(defparameter *hunchentoot-server*
(start (make-instance 'acceptor :port *hunchentoot-port*)))
(princ "Hunchentoot server started on port ")
(princ *hunchentoot-port*) (terpri)

;;; Load any sites here



(in-package :webserver)

;;; Start swank
(load *swank-loader*)
(swank-loader:init)
(swank:create-server :port *swank-port* :dont-close t)
(princ "Loaded Swank on port ")
(princ *swank-port*)(terpri)

;;; Wait and listen for shutdown command
(let ((socket (make-instance 'sb-bsd-sockets:inet-socket
    :type :stream :protocol :tcp)))

;; Listen on a local port for a TCP connection
(sb-bsd-sockets:socket-bind socket #(127 0 0 1) *shutdown-port*)
(sb-bsd-sockets:socket-listen socket 1)

;; When it comes, close the sockets and continue
(multiple-value-bind (client-socket addr port)
(sb-bsd-sockets:socket-accept socket)
(sb-bsd-sockets:socket-close client-socket)
(sb-bsd-sockets:socket-close socket)))

;; Shut down Hunchentoot
(princ "Stopping Hunchentoot...")(terpri)
(stop *hunchentoot-server*)

;; Shut down Swank and anyone else by terminating all threads
(dolist (thread (sb-thread:list-all-threads))
(unless (equal sb-thread:*current-thread* thread)
(sb-thread:terminate-thread thread)))
(sleep 1)
(sb-ext:quit)