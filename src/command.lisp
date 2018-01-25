(in-package :cl-repl)

(defvar *magic-commands* nil)

(defmacro define-magic (name args &body body)
  `(push (list ,(format nil "%~(~a~)" name)
               #'(lambda ,args ,@body))
         *magic-commands*))

(defmacro message-from-magic (message &rest args)
  `(progn
    (format t ,message ,@args)
    "nil"))

(defun invoke-magic (magic &rest args)
  (loop for (name body) in *magic-commands* do
    (when (string= name magic)
      (handler-case
        (return-from invoke-magic
          (apply body args))
        (error ()
          (return-from invoke-magic
            (message-from-magic "Invalid input~p.:~{ ~a~}" (length args) args))))))
  (message-from-magic "Command not found.: ~a" magic))

(defun input-magic-p (input)
  (> (string> input "%") 0))

(defun edit-file-and-read (editor filename)
  (uiop:run-program `(,editor ,filename)
                    :input :interactive
                    :output :interactive)
  (alexandria:read-file-into-string filename))
 
(define-magic edit (&optional filename)
  #+sbcl
  (let ((editor (uiop:getenv "EDITOR")))
    (if (null filename)
      (uiop:with-temporary-file
        (:stream s :pathname p :type "lisp" :prefix "cl-repl-")
        (setf filename (namestring p))
        (format s "#|-*- mode:lisp -*-|#~2%")
        (close s)
        (edit-file-and-read editor filename))
      (edit-file-and-read editor filename)))
  #-sbcl
  (message-from-magic "Not implemented.~%"))
