(in-package :cl-repl)

(defun condition-string (condition)
  (ppcre:regex-replace-all "(?<=\\.) "
                           (ppcre:regex-replace-all "\\s\\s+"
                                                    (format nil "~a" condition) " ")
                           (string #\newline)))

(defun debugger-banner ()
  (format t (color *condition-color* "~a~% [Condition of type ~a]~2%")
          (condition-string *current-condition*) (type-of *current-condition*))
  (format t (color *section-color* "Restarts:~%"))
  (loop :with choices = *invokable-restarts*
        :for choice :in choices
        :for n :to (length choices)
        :do (format t "~2d: [~a] ~a~%"  n (restart-name choice) choice))
  (terpri)
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

(defvar *current-condition*)
(defvar *invokable-restarts*)
(defvar *selected-restart*)
(defvar *backtrace-strings* nil)

(defun debugger (condition hook)
  (let ((*current-condition* condition)
        (*invokable-restarts* (cl-repl/compute-restarts condition)))
    (setf *selected-restart* nil)
    (push (trivial-backtrace:print-backtrace
           condition
           :output nil)
          *backtrace-strings*)
    (debugger-banner)
    (let ((*debugger-hook* hook))
      (repl :level (1+ *debugger-level*) :keymap "debugger")
      (pop *backtrace-strings*)
      (invoke-restart-interactively (or *selected-restart* '*abort)))))

(defun invoke-restart-by-number (args key)
  (declare (ignore args key))
  (format t "~%Restart number: ")
  (finish-output)
  (let ((rl:*done* t))
    (setf n (digit-char-p (rl:read-key))))  
  (if (null n)
      (format t "~%Please input number.~%")
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
  ("\\C-r" #'invoke-restart-by-number)
  ("\\C-t" #'show-backtrace))

