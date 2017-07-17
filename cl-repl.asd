(in-package :cl-user)
(defpackage cl-repl-asd
  (:use #:cl #:asdf))
(in-package :cl-repl-asd)

(defsystem cl-repl
  :version "0.1"
  :author "TANI Kojiro"
  :license "MIT"
  :depends-on (#:alexandria #:cl-ansi-text #:cl-readline)
  :components ((:module "src"
                :components
                  ((:file "repl"
                    :file "util"
                    :file "debugger"
                    :file "completer"))))
  :description "Runtime for CL-REPL.")
