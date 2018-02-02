(in-package :cl-user)
(defpackage :cl-repl
  (:use :cl)
  (:export main
           define-magic
           message-from-magic
           define-color-scheme
           color-scheme
           *default-prompt-function*
           *debugger-prompt-function*))

(defpackage :repl-user
  (:use :cl :cl-repl))
