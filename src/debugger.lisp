(in-package :cl-repl)

(defvar *current-condition*)
(defvar *invokable-restarts*)
(defvar *selected-restart*)
(defvar *backtrace-strings* nil)
(defvar *redisplay-debugger-banner*)
(defparameter *debugger-flush-screen* nil)

(defun condition-string (condition)
  #+sbcl
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition))
  #-sbcl
  (princ-to-string condition))

(defun debugger-banner ()
  (when *debugger-flush-screen* (flush-screen))
  (with-cursor-hidden
    (format t (color *condition-color* "~a~% [Condition of type ~a]~2%")
            (condition-string *current-condition*) (type-of *current-condition*))
    (format t (color *section-color* "Restarts:~%"))
    (loop :with choices = *invokable-restarts*
          :for choice :in choices
          :for n :to (length choices)
          :do (format t "~2d: [~a] ~a~%"  n (restart-name choice) choice))
    (terpri)
    (format t (color *section-color* "Backtrace:~%"))
    (loop :for frame :in (split-lines (car *backtrace-strings*))
          :for n :from 0 :below 2
          :do (format t "~a~%" frame)
          :finally (when (= n 2) (format t " --more--~%")))
    (terpri)
    (format t (color *section-color* "Usage:~%"))
    (format t "  Ctrl+r: select restart. Ctrl+t: show backtrace.~%")
    #+sbcl
    (when (subtypep (type-of *current-condition*) 'sb-ext:step-condition)
      (format t "  Ctrl+o: step-out. Ctrl+x: step-next, Ctrl+o: step-into.~%"))
    (terpri)))

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

#+sbcl
(defun push-backtrace-string ()
  (let ((stack-top-hint sb-debug:*stack-top-hint*))
    (push
      (if stack-top-hint
        (format nil "~{~a~%~}"
          (loop :for f := stack-top-hint :then (sb-di:frame-down f)
                :for n :from 0
                :while f
                :collect (let ((s (make-string-output-stream)))
                           (format s "~2d: " n)
                           (sb-debug::print-frame-call f s)
                           (get-output-stream-string s))))
        "")
      *backtrace-strings*)))

(defun debugger-loop (level)
  (let ((*debugger-level* level))
    (loop :while (catch level (read-eval-print :level level))
          :when *redisplay-debugger-banner*
                :do (progn (debugger-banner) (setf *redisplay-debugger-banner* nil)))))

(defun debugger (condition hook)
  (let ((*current-condition* condition)
        (*invokable-restarts* (cl-repl/compute-restarts condition)))
    (setf *selected-restart* nil
          *redisplay-debugger-banner* nil)
    #+sbcl (push-backtrace-string)
    (debugger-banner)
    (set-keymap "debugger")
    (let ((*debugger-hook* hook))
      (debugger-loop (1+ *debugger-level*)))
    (setf *redisplay-debugger-banner* t)
    (when *debugger-flush-screen* (flush-screen))
    #+sbcl (pop *backtrace-strings*)
    (cl-repl/invoke-restart-interactively *selected-restart*)))

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

(defmacro set-restart (restart-designator)
  (let ((restart (gensym)))
    `(progn
       (setf ,restart (find-restart ,restart-designator))
       (when ,restart
         (setf rl:*done* t)
         (setf *selected-restart* ,restart)
         (throw *debugger-level* nil)))))

(defun step-out (args key)
  (declare (ignore args key))
  #+scbl
  (set-restart 'sb-ext:step-out))

(defun step-next (args key)
  (declare (ignore args key))
  #+sbcl
  (set-restart 'sb-ext:step-next))

(defun step-into (args key)
  (declare (ignore args key))
  #+sbcl
  (set-restart 'sb-ext:step-into))

(define-keymap "debugger" ()
  ("\\C-r" #'select-restart-by-number)
  ("\\C-o" #'step-out)
  ("\\C-x" #'step-next)
  ("\\C-s" #'step-into)
  ("\\C-t" #'show-backtrace))

