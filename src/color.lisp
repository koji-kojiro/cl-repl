(in-package cl-repl)

(defun color (color string)
  (if (null color)
      string
      (format nil "~c[38;5;~am~a~c[0m" #\ESC color string #\ESC)))

(defparameter *default-prompt-color* 40)
(defparameter *debugger-prompt-color* 9)
(defparameter *logo-color* 9)
(defparameter *output-indicator-color* 9)
(defparameter *splash-color* 9)
(defparameter *condition-color* 9)
(defparameter *section-color* 21)
(defparameter *message-color* 248)
