(in-package :cl-repl)

(defvar *magic-commands* nil)

(defmacro define-magic (name args &body body)
  `(push (list ,(format nil "%~(~a~)" name)
               #'(lambda ,args ,@body))
         *magic-commands*))

(defun invoke-magic (magic &rest args)
  (loop for (name body) in *magic-commands* do
    (when (string= name magic)
      (handler-case
        (return-from invoke-magic (apply body args))
        (error () (return-from invoke-magic "Invalid input.")))))
  (format nil "Command not found.: ~a" magic))

(defun input-magic-p (input)
  (> (string> input "%") 0))

(define-magic cd (&optional (dest (uiop:getenv "HOME")))
  (namestring (truename (uiop:chdir dest))))

