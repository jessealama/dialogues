
(in-package :cl-user)

(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(quicklisp:quickload "alexandria")

(push (truename (sb-posix:getcwd)) asdf:*central-registry*)
(asdf:load-system "dialogues")

;; Load CLON
(quicklisp:quickload "com.dvlsoft.clon")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "FILES...")
  (text :contents
	"Kuno -- A theorem prover based on dialogue games")
  (group (:header "Flags (non valued options):")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))
  (group (:header "Built-in valued option types:")
    (group (:header "Limits")
	   (stropt :short-name "t"
		   :long-name "timeout"
		   :description "Spend at most TIME seconds solving any particular problem."
		   :argument-name "TIME"
		   :default-value "5"))))

(defun file-readable? (path)
  (and (probe-file path)
       (streamp (handler-case (open path :direction :probe)
		  (error () nil)))))

(defun red (str)
  (format nil "~C[;31m~a~C[0;m" #\Escape str #\Escape))

(defun cyan (str)
  (format nil "~C[;36m~a~C[0;m" #\Escape str #\Escape))

(defun yellow (str)
  (format nil "~C[;33m~a~C[0;m" #\Escape str #\Escape))

(defun error-message (format-string &rest format-args)
  (format *error-output* (red "Error"))
  (format *error-output* "~a" #\Space)
  (apply #'format *error-output* format-string format-args))

(defmacro help-and-die ()
  `(progn
     (clon:help)
     (clon:exit)))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (when (clon:getopt :short-name "h")
    (help-and-die))
  (unless (clon:remainder)
    (help-and-die))
  (let ((timeout-str (clon:getopt :long-name "timeout"))
	(timeout nil)
	(remainder (clon:remainder)))
    (handler-case (setf timeout (parse-integer timeout-str
					       :junk-allowed nil))
      (error ()
	(error-message "'~a' is not an acceptable value for the timeout option." timeout-str)
	(clon:exit 1)))
    (cond ((rest remainder)
           (error-message "Too many arguments (exactly one is expected).")
           (help-and-die))
          (remainder
           (let ((arg (first remainder)))
             (format *standard-output* "Given argument is \"~a\"." arg)
             (terpri *standard-output*)
             (if (file-readable? arg)
                 (let ((tptp (handler-case (dialogues::parse-tptp (pathname arg))
                               (error () nil))))
                   (if tptp
                       (let ((conjecture (dialogues::conjecture-formula tptp)))
                         (setf conjecture (dialogues::formula conjecture))
                         (format *standard-output* "~a" (dialogues::render tptp))
                         (if conjecture
                             (let ((result (dialogues::intuitionistically-valid--e? conjecture
                                                                                    10
                                                                                    dialogues::*alphabetic-propositional-signature*)))
                               (format *standard-output* "~a" result))))
                       (format *standard-output* "This is not a parsable TPTP file.")))
                 (format *standard-output* "This is an unreadable (or non-existing) file."))
             (terpri *standard-output*))
           (clon:exit 0))
          (t
           (error-message "Not enough arguments (exactly one argument is expected, but zero were given.")
           (help-and-die)))))

(clon:dump "kuno" main)
