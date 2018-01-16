LISP?=sbcl

build:
	$(LISP) --load "cl-repl.asd" \
	     --eval '(ql:quickload :cl-repl)' \
	     --eval '(asdf:make :cl-repl)'
