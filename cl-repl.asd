(defsystem cl-repl
  :version "0.5.1"
  :author "TANI Kojiro"
  :license "GPLv3"
  :depends-on (#:uiop
               #:unix-opts
               #:conium
               #:cl-ppcre
               #:cl-readline)
  :serial t
  :components ((:module "src" :components ((:file "package")
                                           (:file "util")
                                           (:file "color")
                                           (:file "color-scheme")
                                           (:file "highlight")
                                           (:file "keymap")
                                           (:file "pager")
                                           (:file "command")
                                           (:file "shell")
                                           (:file "completer")
                                           (:file "debugger")
                                           (:file "inspector")
                                           (:file "input")
                                           (:file "repl")
                                           (:file "main"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")
