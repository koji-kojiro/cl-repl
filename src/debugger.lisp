(in-package :cl-repl)

(defun condition-string (condition)
  #+sbcl
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition))
  #-sbcl
  (princ-to-string condition))

(defun debugger-banner ()
  (format t (color *condition-color* "~a~% [Condition of type ~a]~2%")
          (condition-string *current-condition*) (type-of *current-condition*))
  (format t (color *section-color* "Restarts:~%"))
  (loop :with choices = *invokable-restarts*
        :for choice :in choices
        :for n :to (length choices)
        :do (format t "~2d: [~a] ~a~%"  n (restart-name choice) choice))
  (terpri)
  (format t (color *section-color* "Bactrace:~%"))
  (loop :for frame :in (split-sequence:split-sequence #\newline (car *backtrace-strings*))
        :for n :from 0 :to 5
        :do (format t "~a~%" frame)
        :finally (when (= n 5) (format t " --more-- ~%")))
  (format t (color *section-color* "Usage:~%"))
  (format t "  Ctrl+r: select restart. Ctrl+t: show backtrace.~2%"))

#+sbcl
(sb-ext:without-package-locks
  (defun break (&optional (format-control "Break") &rest format-arguments)
    (with-simple-restart (*continue "Return from BREAK.")
      (invoke-debugger
       (make-condition 'simple-condition
                       :format-control format-control
                       :format-arguments format-arguments)))
    nil))

(defun cl-repl/compute-restarts (condition)
  (let ((restarts (compute-restarts condition))
        (flag-count 0))
    (or
     (flet ((supplied-by-cl-repl (r)
              (ppcre:scan "CL-REPL" (format nil "~s" r))))
       (loop :for restart :in restarts
             :for count :from 0
             :until (> flag-count 2)
             :when (and (> count 0)
                        (or (supplied-by-cl-repl (nth (1- count) restarts))
                            (supplied-by-cl-repl restart)))
             :do (incf flag-count)
             :when (and (zerop count)
                        (supplied-by-cl-repl restart))
             :do (incf flag-count)
             :collect restart))
     restarts)))

(defun cl-repl/invoke-restart-interactively (restart)
  (unless restart
    (ignore-errors
     (return-from cl-repl/invoke-restart-interactively (invoke-restart '*abort))))
  (flet ((get-new-value () (read-from-string (rl:readline :prompt "value: "))))
    (alexandria:switch ((string-downcase (restart-name restart)) :test #'string=)
      ("store-value" (invoke-restart restart (get-new-value)))
      ("use-value" (invoke-restart restart (get-new-value)))
      (otherwise (invoke-restart-interactively restart)))))

(defvar *current-condition*)
(defvar *invokable-restarts*)
(defvar *selected-restart*)
(defvar *backtrace-strings* nil)

#+sbcl
(defun push-backtrace-string ()
  (let ((stack-top-hint sb-debug:*stack-top-hint*))
    (push
      (if stack-top-hint
        (format nil "~{~a~%~}"
          (loop :for f := stack-top-hint :then (sb-di:frame-down f)
                :for n :from 0
                :until (or (null f) (eql (sb-di:debug-fun-fun (sb-di:frame-debug-fun f)) #'cl-repl::eval-print))
                :collect (let ((s (make-string-output-stream)))
                           (format s "~2d: " n)
                           (sb-debug::print-frame-call f s)
                           (get-output-stream-string s))))
        "")
      *backtrace-strings*)))

(defun debugger (condition hook)
  (let ((*current-condition* condition)
        (*invokable-restarts* (cl-repl/compute-restarts condition)))
    (setf *selected-restart* nil)
    #+sbcl (push-backtrace-string)
    (debugger-banner)
    (let ((*debugger-hook* hook))
      (repl :level (1+ *debugger-level*) :keymap "debugger")
      #+sbcl (pop *backtrace-strings*)
      (cl-repl/invoke-restart-interactively *selected-restart*))))

(defun select-restart-by-number (args key)
  (declare (ignore args key))
  (format t "~%Restart number: ")
  (finish-output)
  (let ((rl:*done* t))
    (setf n (digit-char-p (rl:read-key))))  
  (if (or (null n) (>= n (length *invokable-restarts*)))
      (format t "~%Please input number below ~d.~%" (1- (length *invokable-restarts*)))
      (progn
        (terpri)
        (setf *selected-restart*
              (find-restart (nth n *invokable-restarts*)))
        (throw *debugger-level* nil))))

(defun show-backtrace (args key)
  (declare (ignore args key))
  (terpri)
  (invoke-pager (car *backtrace-strings*))
  (setf rl:*done* t))

(define-keymap "debugger" ()
  ("\\C-r" #'select-restart-by-number)
  ("\\C-t" #'show-backtrace))

