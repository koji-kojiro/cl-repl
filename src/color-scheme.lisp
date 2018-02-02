(in-package :cl-repl)

(defvar *color-schemes* (make-hash-table :test #'equal))

(defun find-color-scheme (name)
  (gethash name *color-schemes*))

(defmacro define-color-scheme (name (&optional parent) &body specs)
  `(setf (gethash ,name *color-schemes*) (cons ,parent ',specs)))

(defun set-color (spec color)
  (eval (read-from-string (format nil "(setf cl-repl::*~a-color* ~a)" spec color))))

(defun color-scheme (name)
  (let ((scheme (find-color-scheme name)))
    (when (car scheme)
      (color-scheme (car scheme)))
    (loop :for (spec color) :in (cdr scheme)
          :do (set-color spec color))))

(define-color-scheme "default" ()
  ("default-prompt" 40)
  ("debugger-prompt" 9)
  ("output-indicator" 9)
  ("condition" 9)
  ("section" 21)
  ("message" 248)
  ("logo" 9))

(define-color-scheme "off" ()
  ("default-prompt" nil)
  ("debugger-prompt" nil)
  ("output-indicator" nil)
  ("condition" nil)
  ("section" nil)
  ("message" nil)
  ("logo" nil))


