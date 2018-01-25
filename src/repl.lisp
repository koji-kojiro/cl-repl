(in-package :cl-repl)
(defconstant +version+ "0.3.4.1")

(defvar *debugger-level* 0)
(define-condition exit-error (error) nil)

(defun exit-with-prompt ()
  (finish-output)
  (alexandria:switch
    ((rl:readline :prompt "Do you really want to exit ([y]/n)? ") :test #'equal)
    ("y" (error 'exit-error))
    ("" (error 'exit-error))
    (nil (progn (format t "~%") (error 'exit-error)))
    ("n" (setf *last-input* "nil"))
    (t (exit-with-prompt))))

(defun print-result (values)
  (format t "~&~a~{ ~s~}~%" (color *output-indicator-color* "[OUT]:") values)
  (finish-output)) 

(let (* ** *** - + ++ +++ / // /// values)
  (defun eval-print (-)
    (setq values
          (multiple-value-list
           (let ((*debugger-hook* #'debugger))
             (eval -))))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (print-result  values))
  (defun repl (&rest argss)
    (in-package :cl-user)
    (loop :with args := nil :do
          (setf *debugger-level* 0)
          (handler-case
            (restart-case
              (eval-print (setq - (read-input)))
              (*abort () :report "Return to CL-REPL's top level.")
              (*retry () :report "Try to evaluate again." (eval-print -)))
            (exit-error () (return))))))


