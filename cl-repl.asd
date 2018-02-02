(defsystem cl-repl
  :version "0.4.0"
  :author "TANI Kojiro"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:unix-opts
               #:split-sequence
               #:trivial-backtrace
               #:cl-readline
               #:cl-annot)
  :serial t
  :components ((:module "src" :components ((:file "package")
                                           (:file "color")
                                           (:file "color-scheme")
                                           (:file "keymap")
                                           (:file "command")
                                           (:file "shell")
                                           (:file "completer")
                                           (:file "debugger")
                                           (:file "input")
                                           (:file "repl")
                                           (:file "main"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")
