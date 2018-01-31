LISP     ?= sbcl
SRC      := ./src/*.lisp
PROG     := cl-repl

.PHONY: build clean indent

build: $(PROG)

clean:
	@$(if $(shell find -name $(PROG)), rm -f $(PROG) && echo removed \'$(PROG)\')	

# format code with `ros fmt`
indent: $(SRC)
	@ros fmt $(SRC)

$(PROG): $(SRC)
	@$(LISP) --load "cl-repl.asd" --eval '(ql:quickload :cl-repl)' --eval '(asdf:make :cl-repl)'


