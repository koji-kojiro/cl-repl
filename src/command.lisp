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

(defun edit-file-and-read (editor filename)
  (message-from-magic "Openning file: ~a~%" filename)
  (uiop:run-program `(,editor ,filename)
                    :input :interactive
                    :output :interactive)
  (message-from-magic "Executing edited code...~%")
  (let ((code (alexandria:read-file-into-string filename)))
    (if (line-continue-p code)
      (message-from-magic  "Error: Unexpected EOF.")
      code)))
 
(define-magic edit (&optional filename)
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
