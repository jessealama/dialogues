;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start-hunchentoot.lisp
;;;;
;;;; Author:  William Bruschi
;;;; Date:    02-14-2009
;;;;
;;;; Starts the dialogue server and swank, then listens for a shutdown
;;;; command on the specified port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'asdf)
(require 'sb-bsd-sockets)

(push "/home/jesse/src/clbuild/systems/" asdf:*central-registry*)
(push "/home/jesse/src/dialogues/" asdf:*central-registry*)

;;; utf-8 input
(setf sb-impl::*default-external-format* :utf-8)

(asdf:oos 'asdf:load-op "dialogues")

(in-package :dialogues)

(defparameter *shutdown-port* 6440)
(defparameter *swank-loader* "/home/jesse/src/clbuild/source/slime/swank-loader.lisp")
(defparameter *swank-port* 4006)

;;; Start the hunchentoot server

(startup-dialogue-server)
(princ "Dialogue server started on port ") (princ *dialogue-server-port*) (terpri)

;;; Start swank
(load *swank-loader*)
(swank-loader:init)
(swank:create-server :port *swank-port* :dont-close t)
(princ "Loaded Swank on port ") (princ *swank-port*) (terpri)

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

;; Shut down the dialogue server
(princ "Stopping the dialogue server...") (terpri)
(shutdown-dialogue-server)

;; Shut down Swank and anyone else by terminating all threads
(dolist (thread (sb-thread:list-all-threads))
  (unless (equal sb-thread:*current-thread* thread)
    (sb-thread:terminate-thread thread)))
(sleep 1)
(sb-ext:quit)