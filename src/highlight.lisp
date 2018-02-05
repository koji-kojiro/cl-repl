(in-package :cl-repl)

(defvar *cl-functions-list*
  (loop :for sym :being :the :external-symbols :of :cl
        :when (handler-case (symbol-function sym) (error () nil))
        :collect (ppcre:regex-replace-all "\\+"
                                          (ppcre:regex-replace-all "\\*" (string-downcase sym) "\\\\*") "\\\\+")))

(defvar *syntax-table*
  (list
   :string (list *string-syntax-color* "\".*?\"")
   :variable (list *variable-syntax-color* "([\\*])\\S+\\1")
   :constant (list *constant-syntax-color* "([\\+])\\S+\\1")
   :keyword (list *keyword-syntax-color* "((?<=\\s)|^):\\S+(?=\\b)")
   :special (list *special-syntax-color* "(?<=\\b)(let|let\\*|lambda)(?=\\b)")
   :function (list  *function-syntax-color*
                    (format nil "(?<=\\b)(~{~a|~})(?=\\b)" *cl-functions-list*))))

(defun map-syntax (syntax text &optional syntax-map)
  (unless syntax-map
    (setf syntax-map (make-list (length text) :initial-element nil)))
  (destructuring-bind (color regex) (getf *syntax-table* syntax)
    (ppcre:do-matches (start end regex text)
      (loop :for n :from start :below end
            :unless (elt syntax-map n)
            :do (setf (elt syntax-map n) (color color (elt text n))))))
  syntax-map)

(defun highlight-text (text)
  (let ((syntax-map))
    (loop :for (syntax val) :on *syntax-table* :by #'cddr
          :do (setf syntax-map (map-syntax syntax text syntax-map)))
    (format nil "~{~a~}"
            (loop :for raw :across text
                  :for colored :in syntax-map
                  :collect (or colored raw)))))

(defun redisplay-with-highlight ()
  (format t "~c[2K~c~a~a~c[~aD"
          #\esc
          #\return
          rl:*display-prompt*
          (highlight-text rl:*line-buffer*)
          #\esc
          (- rl:+end+ rl:*point*))
  (when (= rl:+end+ rl:*point*)
    (format t "~c[1C" #\esc))
  (finish-output))

(defun enable-syntax-highlight ()
  (rl:register-function :redisplay #'redisplay-with-highlight))

(defun disable-syntax-highlight ()
  (rl:register-function :redisplay #'redisplay))
