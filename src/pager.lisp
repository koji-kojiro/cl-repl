(in-package :cl-repl)

(defparameter *pager-command* "less")
(defparameter *pager-minimum-line-count* 10)

(defun probe-pager-command ()
  (handler-case
      (progn
        (uiop:run-program
         (format nil "command -v ~a" *pager-command*))
        t)
    (error () nil)))

(defun invoke-pager (text)
  (if (and (probe-pager-command)
           (> (/ (length (ppcre:all-matches "\\n" text)) 2)
              *pager-minimum-line-count*))
      (uiop:run-program
       (format nil "echo ~s | ~a" text *pager-command*)
       :ignore-error-status t
       :input :interactive
       :output :interactive)
      (format t text)))
