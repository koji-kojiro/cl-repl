(in-package :cl-repl)

(defvar *syntax-table*
  (list
   :string (list *string-syntax-color* "\".*?\"")
   :variable (list *variable-syntax-color* "([\\*])\\S+\\1")
   :constant (list *constant-syntax-color* "([\\+])\\S+\\1")
   :keyword (list *keyword-syntax-color* "((?<=\\s)|^):\\S+(?=\\b)")
   :special (list *special-syntax-color* "(?<=\\b)(let|let\\*|lambda)(?=\\b)")
   ;; not implemented
   :function (list  *function-syntax-color* ".")))

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

(defun get-cursor-pos ()
  (let ((*default-prompt-color*) (*debugger-prompt-color*))
    (+ (length (prompt)) (1- rl:*point*))))

(defvar *rl-last-buffer* "")
(defvar *rl-last-point* 0)

(defun update-highlight ()
  (when (or (string/= *rl-last-buffer* rl:*line-buffer*) (/= *rl-last-point* rl:*point*))
    (format t "~c7~c~a~a~c8"
      #\esc #\return rl:*display-prompt* (highlight-text rl:*line-buffer*) #\esc)
    (finish-output)
    (setf *rl-last-buffer* rl:*line-buffer*)
    (setf *rl-last-point* rl:*point*))
  rl:*line-buffer*)

(rl:register-hook :event #'update-highlight)
