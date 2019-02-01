(in-package :cl-repl)

(defvar *inspected*)
(defvar *inspect-elements*)
(defvar *inspect-named-p*)
(defvar *inspector-state*)
(defvar *inspector-redisplay-banner*)
(defparameter *inspector-flush-screen* nil)

(defun inspector-banner ()
  (when *inspector-flush-screen* (flush-screen))
  (with-cursor-hidden
    #+sbcl
    (destructuring-bind (description named-p elements)
      (multiple-value-list (sb-impl::inspected-parts *inspected*))
      (setf *inspect-named-p* named-p)
      (setf *inspect-elements* elements)
      (format t "~a~%" (color  *condition-color* description))
      (format t (color *section-color* "Slots:~%"))
      (loop :for elm :in elements
            :for n :from 0
            :do (format t "~2d. ~a~%" n elm)))
    (terpri)
    (format t (color *section-color* "Usage:~%"))
    (format t "  q: quit. 0~~9: inspect the numbered slot.~%")
    (format t "  u: move previous. >: evaluate an expression.~2%")))

(defun inspector-process-key ()
  (rl:register-function :redisplay #'(lambda () nil))
  (unwind-protect
    (rl:readline)
    (if *syntax-enabled*
      (enable-syntax)
      (disable-syntax))))

(defun inspect-one (object)
  (let ((*inspected* object)
        (*inspector-state*))
    (inspector-banner)
    (loop :until *inspector-state*
          :when *inspector-redisplay-banner*
                :do (progn
                      (inspector-banner)
                      (setf *inspector-redisplay-banner* nil))
          :do (inspector-process-key))))

(defun inspector (object &rest args)
  (declare (ignore args))
  (set-keymap "inspector")
  (setf *inspector-state* nil
        *inspector-redisplay-banner* nil)
  (format t "~c[?25l" #\escape)
  (catch 'inspector-quit
    (unwind-protect
      (inspect-one object)
      (format t "~c[?25h" #\escape)))
  (when *inspector-flush-screen*
    (flush-screen)
    (if (zerop *debugger-level*)
        (set-keymap "default")
        (progn
          (set-keymap "debugger")
          (debugger-banner)))))

(defun inspector-quit (args key)
  (declare (ignore args key))
  (throw 'inspector-quit nil))

(defun inspector-move-to-previous (args key)
  (declare (ignore args key))
  (setf rl:*done* t
        *inspector-state* t
        *inspector-redisplay-banner* t))

(defun inspector-select-1 (n)
  #+sbcl
  (when (and n (< -1 n (length *inspect-elements*)))
    (let* ((element (nth n *inspect-elements*))
           (value (if *inspect-named-p* (cdr element) element)))
      (if (eq value sb-pcl:+slot-unbound+)
          (format t (color *condition-color* "That slot is unbound.~%"))
          (inspect-one value)))))

(defun inspector-select (args key)
  (declare (ignore args))
  (setf rl:*done* t)
  (inspector-select-1 (digit-char-p key)))

(defun inspector-select-with-input (args key)
  (declare (ignore args key))
  (set-keymap "default")
  (let ((input (rl:readline :prompt "Slot number: ")))
    (handler-case (inspector-select-1 (parse-integer input))
      (error ()
        (format t (color *condition-color* "Invalid input.~%")))))
  (set-keymap "inspector"))

(defun inspector-rep (args key)
  (declare (ignore args key))
  (setf rl:*done* t)
  (if *syntax-enabled*
      (enable-syntax)
      (disable-syntax))
  (set-keymap "default")
  (catch* 0 (read-eval-print))
  (terpri)
  (set-keymap "inspector")
  (inspect-one *inspected*))

(define-keymap "inspector" ()
  (#\q #'inspector-quit)
  (#\u #'inspector-move-to-previous)
  (#\> #'inspector-rep)
  (#\0 #'inspector-select)
  (#\1 #'inspector-select)
  (#\2 #'inspector-select)
  (#\3 #'inspector-select)
  (#\4 #'inspector-select)
  (#\5 #'inspector-select)
  (#\6 #'inspector-select)
  (#\7 #'inspector-select)
  (#\8 #'inspector-select)
  (#\9 #'inspector-select)
  (#\RETURN #'unbind-key))

(defun install-inspector ()
  #+sbcl
  (setf sb-impl::*inspect-fun* #'inspector))
