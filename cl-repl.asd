(defsystem cl-repl
  :version "0.3.3"
  :author "TANI Kojiro"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:cffi
               #:split-sequence
               #:trivial-backtrace
               #:cl-readline)
  :serial t
  :components ((:module "src" :components ((:file "package")
                                           (:file "color")
                                           (:file "command")
                                           (:file "completer")
                                           (:file "debugger")
                                           (:file "input")
                                           (:file "repl"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")
