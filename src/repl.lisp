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

(defun print-result (value)
  (format t "~&~a ~s~%" (color *output-indicator-color* "[OUT]:") value)
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
    (print-result (car values)))
  (defun repl (&rest args)
    (in-package :cl-user)
    (loop :with args := nil :do
             (setf *debugger-level* 0)
             (handler-case
                 (progn
                   (with-simple-restart (*abort "Return to CL-REPL's top level.")
                     (multiple-value-setq (- args) (read-input))
                     (eval-print -)))
               (exit-error () (return))))))


