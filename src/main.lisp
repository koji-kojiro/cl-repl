(in-package :cl-repl)

(defconstant +version+ '0.5.1)

(defvar *logo*
  "  ___  __          ____  ____  ____  __
 / __)(  )    ___ (  _ \\(  __)(  _ \\(  )
( (__ / (_/\\ (___) )   / ) _)  ) __// (_/\\
 \\___)\\____/      (__\\_)(____)(__)  \\____/
")

(defvar *copy* "(C) 2017-2018 TANI Kojiro <kojiro0531@gmail.com>")

(defvar *versions*
  (format nil "cl-repl ~a on ~?~a ~a"
          +version+
          #+ros.script
          "Roswell ~a, "
          #-ros.script
          ""
          #+ros.script
          `(,(ros::version))
          #-ros.script
          nil
          (lisp-implementation-type)
          (lisp-implementation-version)))

(defvar *site-init-path* #P"~/.replrc")
(defun site-init ()
  (unless (probe-file *site-init-path*)
    (return-from site-init))
  (handler-case (load *site-init-path*)
    (error (c)
      (progn
        (format *error-output* "Failed to load ~a, quitting.~%[~a]~%" *site-init-path* c)
        (uiop:quit 1)))))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(opts:define-opts
  (:name :help
   :description "Print this help and exit."
   :short #\h
   :long "help")
  (:name :version
   :description "Show the version info and exit."
   :short #\v
   :long "version")
  (:name :no-init
   :description "Skip to load init file."
   :short #\n
   :long "no-init"))

(defun main (&optional argv &key (show-logo t))
  (multiple-value-bind (options free-args)
      (handler-case
          (if argv (opts:get-opts argv) (opts:get-opts))
        (error ()
          (format t "try `cl-repl --help`.~&")
          (uiop:quit 1)))
    (when-option (options :help)
      (opts:describe
       :prefix "A full-featured Common Lisp REPL implementation.")
      (uiop:quit 0))
    (when-option (options :version)
      (format t "cl-repl v~a~&" +version+)
      (uiop:quit 0))
    (when-option (options :no-init)
      (setf *site-init-path* nil)))
  (enable-syntax)
  (site-init)
  (when show-logo
    (format t (color *logo-color* *logo*)))
  (format t "~a~%~a~2%" *versions* *copy*)
  #+sbcl (sb-ext:enable-debugger)
  (rl:register-function :complete #'completer)
  (in-package :cl-user)
  (repl)
  (rl:deprep-terminal))

