(in-package :cl-repl)

(defvar *magic-commands* nil)

(defmacro define-magic (name args &body body)
  `(progn
     #+sbcl
     (sb-ext:without-package-locks
       (export (intern ,(format nil "%~a" name) :cl) :cl))
     (push (list ,(format nil "%~(~a~)" name)
                 #'(lambda ,args ,@body))
           *magic-commands*)))

(defmacro message-from-magic (message &rest args)
  `(progn
     (format t ,(color *message-color* message) ,@args)
     (finish-output)
     "nil"))

(defun invoke-magic (magic &rest args)
  (loop :for (name body) :in *magic-commands*
        :when (string= name magic)
        :do (return-from invoke-magic  (apply body args)))
  (message-from-magic "Command not found.: ~a" magic))

(defun input-magic-p (&optional input)
  (alexandria:starts-with-subseq "%" input))

(define-magic run (filename &rest args)
  "Execute file in current enviroment."
  (declare (ignore args))
  (if (probe-file filename)
      (format nil "(progn ~a )"
              (alexandria:read-file-into-string filename))
      (message-from-magic "Error: File not found.")))

(defun edit-file-and-read (editor filename)
  (message-from-magic "Openning file: ~a~%" filename)
  (uiop:run-program `(,editor ,filename)
                    :input :interactive
                    :output :interactive)
  (message-from-magic "Evaluating edited code...~%")
  (invoke-magic "%run" filename))

(define-magic edit (&optional filename &rest args)
  "Edit code with text editor specified by $EDITOR."
  (declare (ignore args))
  (let ((editor (uiop:getenv "EDITOR")))
    (if (null filename)
        (uiop:with-temporary-file
            (:stream s :pathname p :type "lisp" :prefix "cl-repl-edit" :suffix "")
          (setf filename (namestring p))
          (message-from-magic "CL-REPL will make a temporary file named: ~a~%" filename)
          (format s "#|-*- mode:lisp -*-|#~2%")
          (close s)
          (edit-file-and-read editor filename))
        (edit-file-and-read editor filename))))

(define-magic time (&rest forms)
  "Alias to (time <form>)."
  (let ((code (format nil "(time ~{ ~a~})" forms)))
    (if (line-continue-p code)
        (message-from-magic "Error: Unexpected EOF.")
        code)))

#+quicklisp
(define-magic load (&rest systems)
  "Alias to (ql:quickload '(<system>...) :silent t)."
  (loop :for system :in systems
        :do (handler-case (ql:quickload (intern system) :silent t)
              (error () (message-from-magic "Failed to load system.: ~a~&" system))))
  "nil")

(define-magic package (&optional (package "cl-user") &rest args)
  "Alias to (in-pacakge <package>)."
  (declare (ignore args))
  (handler-case (progn (setf *package* (find-package (read-from-string package))) "nil")
    (error () (message-from-magic "Failed to change package."))))

(define-magic help (&rest args)
  "List available magic commands and usages."
  (declare (ignore args))
  (loop :for (name body) :in *magic-commands*
        :do (format t "~16,,a~a~%" name (documentation body 'function)))
  "nil")
