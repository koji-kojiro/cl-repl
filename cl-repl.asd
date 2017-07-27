(in-package :cl-user)
(defpackage cl-repl
  (:use #:cl #:asdf))
(in-package :cl-repl)

(defsystem cl-repl
  :version "0.2.2"
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
	       #:split-sequence
	       #-sbcl #:cffi)
  :components ((:module "src" :components ((:file "cl-repl"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")
