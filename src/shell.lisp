(in-package :cl-repl)

(defun shell-command-p (&optional input)
  (string-starts-with input "!"))

(defun run-shell-command (cmd)
  (uiop:run-program (subseq cmd 1) :output *standard-output*))
