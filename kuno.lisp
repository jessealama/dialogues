
(in-package :cl-user)

(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(quicklisp:quickload "trivial-timeout")
(push (truename (sb-posix:getcwd)) asdf:*central-registry*)
(asdf:load-system "dialogues")

;; Load CLON
(quicklisp:quickload "com.dvlsoft.clon")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "TPTP-FILE")
  (text :contents
	"Kuno -- An theorem prover for intuitionistic logic based on dialogue games")
  (group (:header "Flags (non-valued options):")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))
  (group (:header "Built-in valued option types:")
    (group (:header "Limits")
	   (stropt :short-name "t"
		   :long-name "timeout"
		   :description "Spend at most TIME seconds solving any particular problem."
		   :argument-name "TIME"
		   :default-value "30")
           (stropt :short-name "d"
		   :long-name "depth"
		   :description "Consider winning strategies having depth at most DEPTH."
		   :argument-name "DEPTH"
		   :default-value "20"))))

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

(defun parse-tptp-noerror (x)
  (handler-case (dialogues::parse-tptp x)
    (error () nil)))

(defun solve-problem (tptp timeout depth)
  (handler-case
      (trivial-timeout:with-timeout (timeout)
        (dialogues::intuitionistically-valid? tptp depth))
    (trivial-timeout:timeout-error (c)
      (declare (ignore c))
      :timeout)))

(defun result->szs (result)
  (cond ((keywordp result)
         (cond ((eql result :cutoff)
                "ResourceOut")
               ((eql result :timeout)
                "Timeout")
               ((eql result :inappropriate)
                "Inappropriate")
               ((eql result :syntax-error)
                "SyntaxError")
               ((eql result :input-error)
                "InputError")
               ((eql result :error)
                "Error")
               (t
                "Unknown")))
        (result
         "Theorem")
        (t
         "CounterSatisfiable")))

(defun render-error (err)
  (with-output-to-string (rendered)
    (format rendered "% ")
    (loop
       :for c :across (format nil "~a" err)
       :do
       (if (char= c #\Newline)
           (format rendered "~a% " #\Newline)
           (format rendered "~a" c)))))

(defun main ()
  "Entry point for Kuno."
  (clon:make-context)
  (when (clon:getopt :short-name "h")
    (help-and-exit 0))
  (let ((remainder (clon:remainder))
        (timeout-arg (clon:getopt :long-name "timeout"))
        (depth-arg (clon:getopt :long-name "depth"))
        arg
        timeout
        depth
        tptp
        problem
        result
        szs-result
        comment)

    ;; arguments
    (when (null remainder)
      (error-message "Not enough arguments (exactly one argument is expected, but zero were given.")
      (help-and-exit 1))
    (when (rest remainder)
      (error-message "Too many arguments (exactly one is expected).")
      (help-and-exit 1))
    (setf arg (pathname (first remainder)))

    ;; timeout option
    (setf timeout (parse-integer-noerror timeout-arg))
    (unless (integerp timeout)
      (error-message "'~a' is not an acceptable value for the timeout option." timeout-arg)
      (clon:exit 1))

    ;; depth option
    (setf depth (parse-integer-noerror depth-arg))
    (unless (integerp depth)
      (error-message "'~a' is not an acceptable value for the depth option." timeout-arg)
      (clon:exit 1))

    (when (cl-fad:directory-exists-p arg)
      (format *standard-output* "% SZS status ~a for ~a : Argument is a directory rather than a file." (result->szs :input-error) (namestring arg))
      (terpri *standard-output*)
      (clon:exit 1))

    (unless (file-readable? arg)
      (format *standard-output* "% SZS status ~a for ~a : File does not exist or is unreadable." (result->szs :input-error) (namestring arg))
      (terpri *standard-output*)
      (clon:exit 1))

    ;; parse

    (setf tptp (parse-tptp-noerror arg))

    (multiple-value-bind (result comment exit-cleanly-p)
        (handler-case
            (cond ((null tptp)
                   (values :syntax-error "Unparsable TPTP file." nil))
                  ((not (dialogues::has-conjecture-p tptp))
                   (values :inappropriate "No conjecture formula." t))
                  ((dialogues::contains-contradiction-p tptp)
                   (values :inappropriate "At least one occurrence of falsum was found." t))
                  ((dialogues::contains-verum-p tptp)
                   (values :inappropriate "At least one occurrence of verum was found." t))
                  ((dialogues::contains-equation-p tptp)
                   (values :inappropriate "At least one equation was found." t))
                  (t
                   (values (solve-problem tptp timeout depth)
                           nil
                           t)))
          (error (e)
            (let ((rendered-error (render-error e)))
              (values :error (format nil "Internal error~%~a" rendered-error) nil))))
      (setf szs-result (result->szs result))
      (format *standard-output* "% SZS status ~a for ~a" szs-result (namestring arg))
      (when (stringp comment)
        (format *standard-output* " : ~a" comment))
      (terpri *standard-output*)
      (clon:exit (if exit-cleanly-p 0 1)))))

(clon:dump "kuno" main)
