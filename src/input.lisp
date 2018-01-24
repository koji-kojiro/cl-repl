(in-package :cl-repl)

(defvar *last-input* nil)

(defun current-package ()
  (let ((nickname (car (package-nicknames *package*))))
    (if nickname nickname (package-name *package*)))) 

(defvar *default-prompt-function*
  #'(lambda ()
      (format nil "~a> " (current-package))))

(defvar *debugger-prompt-function*
  #'(lambda () (format nil "[~a]> " *debugger-level*)))

(defun prompt (&key (multiline-p nil))
  (let* ((prompt-function (if (zerop *debugger-level*)
                              *default-prompt-function*
                              *debugger-prompt-function*))
         (color (if (zerop *debugger-level*)
                    *default-prompt-color*
                    *debugger-prompt-color*))
         (prompt-string (funcall prompt-function)))
    (if multiline-p
        (setf prompt-string
              (format nil "~V@{.~}" (1- (length prompt-string)) :dummy)))
    (color color prompt-string)))

(defun line-continue-p (string)
  (let ((retval nil))
    (handler-case (read-from-string string)
      (error () (setf retval t)))
    retval))

(defun check-input (input)
  (alexandria:switch
      (input :test #'equal)
    ("" (setf *last-input* "nil"))
    (nil (progn (format t "~%") (exit-with-prompt)))
    (t (setf *last-input* input))))

(defun read-input1 (&key (multiline-p nil))
  (finish-output)
  (rl:readline :prompt (prompt :multiline-p multiline-p)
               :add-history (zerop *debugger-level*)))

(defun read-input ()
  (let ((input (read-input1)))
    (check-input input))
  (loop
    (if (line-continue-p *last-input*)
        (setf *last-input*
              (format nil "~a~%~a"
                      *last-input*
                      (read-input1 :multiline-p t)))
        (return)))
  (read-from-string *last-input*))

