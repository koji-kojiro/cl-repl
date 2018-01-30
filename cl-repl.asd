(defsystem cl-repl
  :version "0.3.3"
  :author "TANI Kojiro"
  :license "MIT"
  :depends-on (#:alexandria
	       #:uiop
	       #:cl-fad
	       #:cl-ppcre
	       #:cl-ansi-text
	       #:cl-readline
	       #:alexandria
	       #:inferior-shell
	       #:trivial-timeout
	       #:trivial-documentation
	       #:split-sequence)
  :components ((:module "src" :components ((:file "cl-repl"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")
