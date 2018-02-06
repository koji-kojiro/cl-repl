(in-package :cl-repl)

(defvar *last-input* nil)

(defun current-package ()
  (let ((nickname (car (package-nicknames *package*))))
    (if nickname nickname (package-name *package*)))) 

(defvar *default-prompt-function*
  #'(lambda ()
      (format nil "~a> " (current-package))))

(defvar *debugger-prompt-function*
  #'(lambda () (format nil "[~a]~a> " *debugger-level* (current-package))))

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
              (format nil "~V@{.~} " (1- (length prompt-string)) :dummy)))
    (color color prompt-string)))

(defun line-continue-p (string)
  (let ((*read-eval* nil))
    (handler-case (progn (read-from-string string) nil)
      (end-of-file () t))))

(defun check-input (input)
  (when (input-magic-p input)
    (setf *last-input*
          (apply #'invoke-magic
                 (split-sequence:split-sequence
                  #\SPACE
                  input
                  :remove-empty-subseqs t)))
    (return-from check-input))
  (when (shell-command-p input)
    (setf *last-input* "nil")
    (run-shell-command input)
    (return-from check-input))
  (alexandria:switch
      (input :test #'equal)
    ("" (setf *last-input* "nil"))
    (nil (progn (format t "~%") (exit-with-prompt)))
    (t (setf *last-input* input))))

(defun read-input1 (&key (multiline-p nil))
  (finish-output)
  (rl:readline :prompt (prompt :multiline-p multiline-p)
               ;                :add-history (zerop *debugger-level*)))
               :add-history t))

(defun read-input ()
  (let ((input (read-input1)))
    (check-input input))
  (loop
    (if (line-continue-p *last-input*)
        (let ((input (read-input1 :multiline-p t)))
          (when (null input)
            (terpri)
            (return-from read-input "nil"))
          (setf *last-input* (format nil "~a~%~a" *last-input* input)))
        (return-from read-input *last-input*))))

