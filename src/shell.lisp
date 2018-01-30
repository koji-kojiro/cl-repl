(in-package :cl-repl)

(defun shell-command-p (&optional input)
  (alexandria:starts-with-subseq "!" input))

(defun run-shell-command (cmd)
  (uiop:run-program (subseq cmd 1) :output *standard-output*))
