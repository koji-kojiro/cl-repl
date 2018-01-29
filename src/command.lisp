(in-package :cl-repl)

(defvar *magic-commands* nil)

(defmacro define-magic (name args &body body)
  `(push (list ,(format nil "%~(~a~)" name)
               #'(lambda ,args ,@body))
         *magic-commands*))

(defmacro message-from-magic (message &rest args)
  `(progn
     (format t ,(color *message-color* message) ,@args)
     "nil"))

(defun invoke-magic (magic &rest args)
  (loop for (name body) in *magic-commands* do
           (when (string= name magic)
             (return-from invoke-magic
               (apply body args))))
  (message-from-magic "Command not found.: ~a" magic))

(defun input-magic-p (&optional input)
  (alexandria:starts-with-subseq "%" input))

(defun read-from-file (filename)
  (unless (probe-file filename)
    (return-from read-from-file
      (message-from-magic "Error: File not found.")))
  (let ((code (alexandria:read-file-into-string filename)))
    (setf code (format nil "(progn ~a)" code))
    (if (line-continue-p code)
        (message-from-magic  "Error: Unexpected EOF.")
        code)))

(defun edit-file-and-read (editor filename)
  (message-from-magic "Openning file: ~a~%" filename)
  (uiop:run-program `(,editor ,filename)
                    :input :interactive
                    :output :interactive)
  (message-from-magic "Executing edited code...~%")
  (read-from-file (pathname filename)))

(define-magic edit (&optional filename &rest args)
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

(define-magic run (filename &rest args)
  (declare (ignore args))
  (read-from-file (pathname filename)))

#+quicklisp
(define-magic load (&rest systems)
  (loop :for system :in systems
        :do (handler-case (ql:quickload (intern system) :silent t)
              (error () (message-from-magic "Failed to load system.: ~a~&" system))))
  "nil")

(define-magic package (&optional (package "cl-user") &rest args)
  (declare (ignore args))
  (handler-case (progn (setf *package* (find-package (read-from-string package))) "nil")
    (error () (message-from-magic "Failed to change package."))))
