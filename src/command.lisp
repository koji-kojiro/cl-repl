(in-package :cl-repl)

(defvar *magic-commands* nil)

(defmacro define-magic (name args &body body)
  (let ((symname (format nil "%~(~a~)" name)))
    `(progn
       #+sbcl
       (sb-ext:without-package-locks
         (export (intern ,symname :cl) :cl)
         (shadow (intern ,symname :cl) :cl))
       (push (list ,symname #'(lambda ,args ,@body))
             *magic-commands*))))

(defmacro message-from-magic (message &rest args)
  `(progn
     (format t ,(color *message-color* message) ,@args)
     (finish-output)
     "nil"))

(defun invoke-magic (magic &rest args)
  (loop :for (name body) :in *magic-commands*
        :when (string= name magic)
        :do (return-from invoke-magic
              (handler-case (apply body args)
                (error (c) (message-from-magic "Error: ~a" c)))))
  (message-from-magic "Command not found.: ~a" magic))

(defun input-magic-p (&optional input)
  (alexandria:starts-with-subseq "%" input))

(define-magic run (filename &rest args)
  "Execute file in current enviroment."
  (declare (ignore args))
  (if (probe-file filename)
      (let ((code (format nil "(progn ~a )" (alexandria:read-file-into-string filename))))
        (if (line-continue-p code)
            (message-from-magic "Error: Unexpected EOF.")
            code))
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

(define-magic cd (&optional (dest (uiop:getenv "HOME")) &rest args)
  "Change working directory."
  (declare (ignore args))
  (handler-case
      (progn
        (setf dest (truename dest))
        (uiop:chdir dest)
        (setf *default-pathname-defaults* dest)
        (format nil "~s"dest))
    (error () (message-from-magic "No such directory."))))

(define-magic time (&rest forms)
  "Alias to (time <form>)."
  (let ((code (format nil "(time ~{ ~a~})" forms)))
    (if (line-continue-p code)
        (message-from-magic "Error: Unexpected EOF.")
        code)))

#+quicklisp
(define-magic load (&rest systems)
  "Alias to (ql:quickload '(<system>...) :silent t)."
  (loop :repeat (length systems)
        :for system := (pop systems) :while system
        :do (handler-case
                (progn
                  (ql:quickload (intern system :keyword) :silent t)
                  (message-from-magic "Loaded.: `~a`" system))
              (error (c) (message-from-magic "Failed to load system.: `~a`: ~a" system c)))
        :when (car systems) :do (terpri)
        :finally (return "nil")))

(define-magic package (&optional (package "cl-user") &rest args)
  "Alias to (in-pacakge <package>)."
  (declare (ignore args))
  (handler-case
      (let ((p (current-package)))
        (setf *package* (find-package (read-from-string package)))
        (message-from-magic "Package changed.: From ~(`~a` into `~a`~)" p (current-package)))
    (error () (message-from-magic "Failed to change package."))))

(flet ((probe-command (cmd)
         (handler-case
             (progn
               (uiop:run-program
                (format nil "command -v ~a" cmd))
               t)
           (error () nil)))
       (execute-foreign (cmd args)
         (write-to-string
          (caddr
           (multiple-value-list
            (uiop:run-program
             (format nil "~a \"~{~a ~}\"" cmd
                     (mapcar #'(lambda (a)
                                 (ppcre:regex-replace-all "\"" a "\\\""))
                             args))
             :output :interactive
             :input :interactive
             :error-output :interactive
             :ignore-error-status t))))))
  (when (probe-command "python")
    (define-magic python (&rest args)
      "Execute line with Python."
      (execute-foreign "python -c" args)))
  (when (probe-command "ruby")
    (define-magic ruby (&rest args)
      "Execute line with Ruby."
      (execute-foreign "ruby -e" args)))
  (when (probe-command "perl")
    (define-magic perl (&rest args)
      "Execute line with Perl."
      (execute-foreign "perl -e" args))))

(define-magic doc (target &rest args)
  "Show description of given object."
  (declare (ignore args))
  (handler-case
      (let ((s (make-array '(0)
                           :element-type 'base-char
                           :fill-pointer 0
                           :adjustable t)))
        (with-output-to-string (sb s)
          (describe (read-from-string target) sb))
        (invoke-pager s)
        "nil")
    (error () (message-from-magic "No description given on `~a.`" target))))

(define-magic cls (&rest args)
  "Clear screen."
  (declare (ignore args))
  (uiop:run-program "clear" :output *standard-output*)
  (read-input))

(define-magic help (&rest args)
  "List available magic commands and usages."
  (declare (ignore args))
  (loop :for (name body) :in *magic-commands*
        :do (format t "~16,,a~a~%" name (documentation body 'function)))
  "nil")


