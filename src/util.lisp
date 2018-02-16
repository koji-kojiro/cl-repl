(in-package :cl-repl)

(defun split-lines (text)
  (ppcre:split "\\n+" text))

(defun split-space (text)
  (ppcre:split "\\s+" text))

(defun string-starts-with (str prefix)
  (when (>= (length str) (length prefix))
    (string= str prefix :start1 0 :end1 (length prefix))))
  
(defun string-ends-with (str suffix)
 (when (>= (length str) (length suffix))
   (string= str suffix :start1 (- (length str) (length suffix)))))

(defmacro string-case (str &rest forms)
  `(case (intern ,str)
     ,@(loop :for (s f) :in forms
             :if (stringp s) :collect `(,(intern s) ,f)
             :else :collect `(,s ,f))))

(defmacro with-cursor-hidden (&body body)
  `(unwind-protect
     (progn
       (format t "~c[?25l" #\esc)
       (finish-output)
       ,@body)
     (progn
       (format t "~c[?25h" #\esc)
       (finish-output))))

(defun flush-screen ()
  (with-cursor-hidden
    (format t "~c[2J~@*~c[;H" #\esc)
    (finish-output)))




