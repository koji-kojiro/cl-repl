(in-package :cl-repl)

(defvar *debugger-level* 0)

(defun exit-with-prompt ()
  (finish-output)
  (when (zerop *debugger-level*)
    (alexandria:switch
        ((rl:readline :prompt "Do you really want to exit ([y]/n)? ")
         :test #'equal)
      (nil (terpri)) ("") ("y")
      ("n" (return-from exit-with-prompt (setf *last-input* "nil")))
      (t (exit-with-prompt))))
  (throw *debugger-level* nil))

(defun print-result (values)
  (format t "~&~a~{ ~s~}~%" (color *output-indicator-color* "[OUT]:") values)
  (finish-output) t) 

(defun eval-print (-)
  (let ((*debugger-hook* #'debugger))
    (setq values
          (multiple-value-list
           (eval -)))
    (setq +++ ++ /// // *** (car ///)
          ++ + // / ** (car //)
          + - / values * (car /))
    (print-result values)))

(defun read-eval-print ()
  (restart-case
      (eval-print (setq - (read-input)))
    (*abort () :report "Deduce debugger level." t)
    (*exit () :report "Exit CL-REPL." (throw 0 nil))
    (*retry () :report "Try evaluating again." (eval-print -))))

(defun repl (&key package (level 0) (keymap "default"))
  (when (null package) (in-package :cl-user))
  (loop :with *debugger-level* := level
        :do (set-keymap keymap)
        :while (catch level (read-eval-print))))
