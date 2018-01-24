(in-package :cl-repl)

(defun debugger-loop (choices condition)
  (format t (color *section-color* "~%restarts:~%"))
  (let ((n (length choices)) (i))
    (do ((c choices (cdr c)) (i 0 (+ i 1)))
        ((null c))
      (format t (color *restart-candidates-color* "~2d. [~a] ~a~%") i (restart-name (car c)) (car c)))
    (print-backtrace condition n)
    (terpri)
    (loop
      (let ((input (read-input)))
        (if (typep input `(integer 0 ,n))
            (progn
              (setf i input)
              (if (= i n)
                  (progn (show-backtrace condition))
                  (return)))
            (eval-print input))))
    (find-restart (nth i choices))))

(defun show-backtrace (condition)
  (trivial-backtrace:print-backtrace condition))

(defun get-backtrace (condition)
  (split-sequence:split-sequence #\NEWLINE
                                   (trivial-backtrace:print-backtrace
                                    condition
                                    :output nil)
                                 :remove-empty-subseqs t))

(defun print-backtrace (condition n)
  (finish-output)
  (let ((backtrace (get-backtrace condition)))
    (loop for b in backtrace
          for i from 0
          while (< i 5)
          do (format t (color *backtrace-color* " ~a~%") b)
          initially (format t (color *section-color* "~%backtrace:~%"))
          finally (format t " --more-- (type ~d)~%" n))))

(defun debugger (condition me-or-my-encapsulation)
  (incf *debugger-level*)
  (format t (color *condition-color* "~a~%  [Condition of type ~a]~%") condition (type-of condition))
  (let ((restart (debugger-loop (compute-restarts) condition)))
    (if (not restart) (error "The debugger got an error."))
    (let ((*debugger-hook* me-or-my-encapsulation))
      (decf *debugger-level*)
      (invoke-restart-interactively restart))))

