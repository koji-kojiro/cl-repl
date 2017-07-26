(in-package #:cl-user)
(defpackage #:cl-repl
  (:use #:cl))
(in-package #:cl-repl)

(defconstant +version+ :0.2.1)

(defun bold (string)
  (format nil "~C[~Am~A~C[0m" (code-char #o33) "1" string (code-char #o33)))

(defvar *splash*
  (bold
   (cl-ansi-text:red
    "  ___  __          ____  ____  ____  __
 / __)(  )    ___ (  _ \\(  __)(  _ \\(  )
( (__ / (_/\\ (___) )   / ) _)  ) __// (_/\\
 \\___)\\____/      (__\\_)(____)(__)  \\____/
")))


(defvar *last-input* nil)
(defvar *history* nil)

(define-condition exit-error (error) nil)

(defun exit-with-prompt ()
  (finish-output)
  (alexandria:switch
   ((rl:readline :prompt "Do you really want to exit ([y]/n)? ") :test #'equal)
   ("y" (error 'exit-error))
   (nil (progn (format t "~%") (error 'exit-error)))
   ("n" (setf *last-input* "nil"))
   (t (exit-with-prompt))))

(defun prompt ()
  (format nil "~a> " (car (package-nicknames *package*))))

(defun newline (prompt)
  (format nil "~a "
          (apply
           #'concatenate 'string
           (loop
              repeat (1- (length prompt))
              collect "."))))

(defun read-input (&key (prompt (prompt)))
  (let ((input (rl:readline :prompt (bold (cl-ansi-text:green prompt)) :add-history t)))
    (if (not input)
        (progn
          (format t "~%")
          (exit-with-prompt))
        (setf *last-input* input)))
  (loop
     (let ((left (count "(" *last-input* :test #'string-equal))
           (right (count ")" *last-input* :test #'string-equal)))
       (if (>= right left) (return)))
     (let ((input (rl:readline :prompt (newline prompt) :add-history t)))
       (if (not input)
           (progn
             (format t "~%")
             (return))
           (setf *last-input* (concatenate 'string *last-input* (format nil "~%") input)))))
  (read-from-string *last-input*))

(defun write-output (output)
  (push *last-input* *history*)
  (if (> (length (princ-to-string output)) 0)
      (format t "~a ~a~%" (bold (cl-ansi-text:red "[OUT]:"))  output))
  (finish-output))

(defun print-condition (condition)
  (princ (bold (format  nil (cl-ansi-text:red "Error: ~a~%") condition))) "")

(defun debugger (condition)
  (print-condition condition)
  (format t
          (cl-ansi-text:blue (bold "~a~%~a~%~a~%"))
          "[0]: Try evaluating again."
          "[1]: Return to top level."
	  #+sbcl (format nil "~a~%" "[2]: Edit code.")
	  #-sbcl "")
  (finish-output)
  (let ((last-input *last-input*))
    (loop
       (handler-case
           (let ((input (read-input :prompt "DEBUG> ")))
             (handler-case
                 (alexandria:switch (input)
                                    (0 (progn
                                         (write-output
                                          (eval (read-from-string last-input))) (return)))
                                    (1 (return))
                                    #+sbcl(2 (progn
                                               (write-output
						(if *file-to-open*
						    (edit-magic (list *file-to-open*))
						    (edit-magic nil :code last-input)))
                                               (return)))
                                    (t (write-output (eval input))))
               (error (condition)
                 (progn
                   (if (eq input 2)
                       (setf last-input *last-input*))
                   (print-condition condition)))))
	 (exit-error () (progn (finish-output) (return ""))))
       (finish-output))))

(defun common-prefix (items)
  (subseq
   (car items)
   0
   (apply
    #'min
    (mapcar
     #'(lambda (i) (or (mismatch (car items) i) (length i)))
     (cdr items)))))

(defun package-prefix (str)
  (cond
    ((let ((pos (search "::" str)))
       (when pos
         (list (subseq str (+ pos 2)) (subseq str 0 pos) nil))))
    ((let ((pos (position #\: str)))
       (when pos
         (list
          (subseq str (+ pos 1))
          (if (zerop pos)
              "KEYWORD"
              (subseq str 0 pos))
          t))))
    (t (list str nil nil))))

(defun completer (text start end)
  (declare (ignore start end))
  (if (string-equal text "")
      (return-from completer '(" ")))
  (let ((text (string-upcase text))
        (els))
    (flet ((body (sym text prefix)
             (let ((name (string sym)))
               (when (eql 0 (search text name))
                 (push (format nil "~(~a~a~)" prefix name)
                       els)))))
      (destructuring-bind (symbol-name package external-p)
          (package-prefix text)
        (when (and package (not (find-package package)))
          (return-from completer nil))
        (cond ((and package external-p)
               (do-external-symbols (sym package)
                 (body sym symbol-name
                       (if (equal (package-name :keyword)
                                  (package-name package))
                           ":"
                           (format nil "~a:" package)))))
              (package
               (do-symbols (sym package)
                 (body sym symbol-name (format nil "~a::" package))))
              (t
               (do-symbols (sym *package*)
                 (body sym symbol-name ""))
               (dolist (package (list-all-packages))
                 (body (format nil "~a:" (package-name package))
                       symbol-name "")
                 (dolist (package-name (package-nicknames package))
                   (body (format nil "~a:" package-name)
                         symbol-name "")))))))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

#+sbcl (rl:register-function :complete #'completer)
#-sbcl (progn
         (cffi:define-foreign-library readline
             (:darwin (:or "libreadline.dylib"))
           (:unix (:or "libreadline.so.6.3"
                       "libreadline.so.6"
                       "libreadline.so"))
           (t (:default "libreadline")))
         (cffi:use-foreign-library readline)
         (setf rl::*attempted-completion-function*
               (rl::produce-callback
                (lambda (text start end)
                  (prog1
                      (rl::to-array-of-strings
                       (funcall #'completer text start end))
                    (setf rl::*attempted-completion-over* t)))
                :pointer
                (:string :int :int))))

(defun introspectionp (input)
  (alexandria:starts-with-subseq "?" input))

(defun shell-commandp (input)
  (alexandria:starts-with-subseq "!" input))

(defun magic-commandp (input)
  (alexandria:starts-with-subseq "%" input))

#+sbcl
(defparameter *file-to-open* nil)

#+sbcl
(defun create-tmpfile (&optional (code nil))
  (cl-fad:with-output-to-temporary-file (tmp :template "/tmp/common-lisp-edit-%.lisp")
    (if code (princ code tmp))))

#+sbcl
(defun open-editor (filepath)
  (let ((editor (uiop:getenv "EDITOR")))
    (if (not editor)
	(setf editor "vi"))
    (format t "Openning file: ~a~%" filepath)
    (finish-output)
    (inferior-shell:run/interactive
     (format nil "~a ~a" editor (namestring filepath))))
  (with-open-file (s filepath :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s) buf)))

#+sbcl
(defun edit-magic (args &key (code nil))
  (if (first args)
      (setf *file-to-open* (first args))
      (setf *file-to-open* nil))
  (let ((tmp (if *file-to-open* *file-to-open* (create-tmpfile code))))
    (let ((edited (open-editor tmp)))
      (setf *last-input* edited)
      (format t "Executing edited code...~%~a~%" (bold (cl-ansi-text:blue edited)))
      (unless *file-to-open* (delete-file tmp))))
  (eval (read-from-string (concatenate 'string "(progn " *last-input* ")"))))

(defun load-magic (args)
  (mapcar (lambda (x) (ql:quickload x :silent t)) args) "")

(defun print-second (time)
  (format t "~a sec~%" (float (/ time internal-time-units-per-second))))

(defun time-magic (args)
  (let ((results nil))
    (handler-case
        (trivial-timeout:with-timeout (10)
          (loop repeat 100 do
               (let ((code (read-from-string (format nil "~{~a~^ ~}" args)))
                     (start (get-internal-real-time)))
                 (eval code)
                 (setf results
                       (cons
                        (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second
                           0.001)
                        results)))))
      (trivial-timeout:timeout-error ()))
    (let ((ntimes (length results)))
      (if (zerop ntimes)
          (error 'trivial-timeout:timeout-error)
          (format t "~a loops, average: ~f ms, best: ~f ms~%"
                  ntimes
                  (alexandria:mean results)
                  (apply #'min results))))) "")

(defun save-magic (args)
  (let ((fname (first args)))
    (if (not fname)
        (error "Empty file name."))
    (with-open-file (out fname :direction :output)
      (dolist (line (reverse *history*))
        (if (not (or (shell-commandp line)
                     (magic-commandp line)))
            (format out "~a~%" line))))) "")

(defun introspection ()
  (let ((object (subseq *last-input* 1)))
    (if (not object)
        (return-from introspection ""))
    (let ((spec (car (trivial-documentation:symbol-definitions (read-from-string object)))))
      (let ((aspec (alexandria:plist-alist spec)))
        (if (not (cdr (assoc :kind aspec)))
            (return-from introspection ""))
        (format t "~a~a~%" (bold (cl-ansi-text:red "Type: "))
                (string-downcase (princ-to-string (cdr (assoc :kind aspec)))))
        (if (assoc :lambda-list aspec)
            (format t "~a~a~%" (bold (cl-ansi-text:red "Args: "))
                    (string-downcase (princ-to-string (cdr (assoc :lambda-list aspec))))))
        (if (assoc :value aspec)
            (format t "~a~a~%" (bold (cl-ansi-text:red "Value: ")) (cdr (assoc :value aspec))))
        (if (cdr (assoc :documentation aspec))
            (let ((doc (cdr (assoc :documentation aspec))))
              (format t "~a~w~%" (bold (cl-ansi-text:red "Docstring: ")) doc)))))) "")

(defun magic ()
  (let ((inputs (split-sequence:split-sequence #\space (subseq *last-input* 1))))
    (let ((cmd (car inputs)) (args (cdr inputs)))
      (alexandria:switch
       (cmd :test #'equal)
       ("load" (load-magic args))
       ("time" (time-magic args))
       #+sbcl("edit" (edit-magic args))
       ("save" (save-magic args))
       (t "")))))

(defun shell ()
  (inferior-shell:run (subseq *last-input* 1)) "")

(let (* ** *** - + ++ +++ / // /// values)
  (defun eval-input (-)
    (cond
      ((introspectionp *last-input*) (introspection))
      ((magic-commandp *last-input*) (magic))
      ((shell-commandp *last-input*) (shell))
      (t (progn
           (setq
            values
            (multiple-value-list
             (eval -)))
           (setq +++ ++ /// // *** (car ///)
                 ++ + // / ** (car //)
                 + - / values * (car /)) )))))

(defun repl (&key (load nil))
  (in-package :cl-user)
  (macrolet ((print-with-handler (&rest body)
	       `(handler-case
		    (write-output ,@body)
		  (exit-error () (return-from repl))
		  (condition (c) (debugger c)))))
    (if load (let ((*file-to-open* load))
	       (print-with-handler
		(with-open-file (s load :direction :input)
		  (let ((buf (make-string (file-length s))))
		    (read-sequence buf s)
		    (setf *last-input* buf)
		    (eval-input (read-from-string (concatenate 'string "(progn " buf ")"))))))))
    (loop (print-with-handler (eval-input (read-input))))))
