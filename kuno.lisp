
(in-package :cl-user)

(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(quicklisp:quickload "alexandria")
(quicklisp:quickload "trivial-timeout")
(push (truename (sb-posix:getcwd)) asdf:*central-registry*)
(asdf:load-system "dialogues")

;; Load CLON
(quicklisp:quickload "com.dvlsoft.clon")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "FILES...")
  (text :contents
	"Kuno -- An theorem prover for intuitionistic logic based on dialogue games")
  (group (:header "Flags (non valued options):")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))
  (group (:header "Built-in valued option types:")
    (group (:header "Limits")
	   (stropt :short-name "t"
		   :long-name "timeout"
		   :description "Spend at most TIME seconds solving any particular problem."
		   :argument-name "TIME"
		   :default-value "30"))))

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

(defmacro help-and-exit (&optional (exit-code 0))
  `(progn
     (clon:help)
     (clon:exit ,exit-code)))

(defun parse-integer-noerror (x)
  (handler-case (parse-integer x :junk-allowed nil)
    (error () nil)))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (when (clon:getopt :short-name "h")
    (help-and-exit 0))
  (let ((remainder (clon:remainder)))
    (when (null remainder)
      (error-message "Not enough arguments (exactly one argument is expected, but zero were given.")
      (help-and-exit 1))
    (when (rest remainder)
        (error-message "Too many arguments (exactly one is expected).")
        (help-and-exit 1))
    (let* ((timeout-str (clon:getopt :long-name "timeout"))
           (timeout (parse-integer-noerror timeout-str)))
      (unless (integerp timeout)
        (error-message "'~a' is not an acceptable value for the timeout option." timeout-str)
	(clon:exit 1))
      (let ((arg (first remainder)))
        (format *standard-output* "Given argument is \"~a\"." arg)
        (terpri *standard-output*)
        (if (file-readable? arg)
            (let ((tptp (handler-case (dialogues::parse-tptp (pathname arg))
                          (error () nil))))
              (if tptp
                  (if (dialogues::has-conjecture-p tptp)
                      (let ((problem (dialogues::problematize tptp)))
                        (setf problem (dialogues::equivalence->conjunction problem))
                        (setf problem (dialogues::binarize problem))
                        (format *standard-output* "~a" problem)
                        (terpri *standard-output*)
                        (let ((result (handler-case
                                          (trivial-timeout:with-timeout (timeout)
                                            (dialogues::intuitionistically-valid--e-no-pro-repeats? problem
                                                                                                    20
                                                                                                    dialogues::*alphabetic-propositional-signature*))
                                        (trivial-timeout:timeout-error (c)
                                          (declare (ignore c))
                                          :timeout))))
                          (format *standard-output* "~a" result)))
                      (error-message "No conjecture formula!"))
                  (format *standard-output* "This is not a parsable TPTP file.")))
            (format *standard-output* "This is an unreadable (or non-existing) file."))
        (terpri *standard-output*)
        (clon:exit 0)))))

(clon:dump "kuno" main)
