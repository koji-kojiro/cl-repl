(in-package :cl-repl)

(defun debugger-banner ()
   (format t (color *condition-color* "~a~%  [Condition of type ~a]~2%")
            *current-condition* (type-of *current-condition*))
   (format t (color *section-color* "Restarts:~%"))
   (loop :with choices = *invokable-restarts*
         :for choice :in choices
         :for n :to (length choices)
         :do (format t "~2d: [~a] ~a~%"  n (restart-name choice) choice))
   (terpri)
   (format t (color *section-color* "Usage:~%"))
   (format t "  Ctrl+r: select restart. Ctrl+t: show backtrace.~2%"))

(defvar *current-condition*)
(defvar *invokable-restarts*)
(defvar *selected-restart* nil)

(defun debugger (condition hook)
  (setf *current-condition* condition)
  (setf *invokable-restarts* (compute-restarts condition))
  (debugger-banner)
  (let ((*debugger-hook* hook))
    (repl :level (1+ *debugger-level*) :keymap "debugger")
    (invoke-restart-interactively (or *selected-restart* '*abort))))

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
  (trivial-backtrace:print-backtrace *current-condition*)
  (setf rl:*done* t))

(define-keymap "debugger" ()
  ("\\C-r" #'invoke-restart-by-number)
  ("\\C-t" #'show-backtrace))

