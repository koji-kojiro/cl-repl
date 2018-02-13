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

(defvar *output-indicator-function*
  #'(lambda () "[OUT]: "))

(defun print-result (values)
  (format t "~&~a~{~s ~}~%" (color *output-indicator-color* (funcall *output-indicator-function*)) values)
  (finish-output) t) 

(defun eval-print (-)
  (setq values
        (multiple-value-list
          (eval -)))
  (setq +++ ++ /// // *** (car ///)
        ++ + // / ** (car //)
        + - / values * (car /))
  (print-result values))

(defmacro with-extra-restarts (form &rest restarts)
  `(restart-case ,form
     (*abort () :report "Deduce debugger level." t)
     (*exit () :report "Exit CL-REPL." (throw 0 nil))
     ,@restarts))

(defun read-eval-print ()
  (with-extra-restarts
    (eval-print (setq - (read-from-string (read-input))))
    (*retry () :report "Try evaluating again."
      (with-extra-restarts (eval-print -)))))

(defun repl (&key package (level 0) (keymap "default"))
  (loop :with *debugger-level* := level
        :do (set-keymap keymap)
        :while (catch level (read-eval-print))))
