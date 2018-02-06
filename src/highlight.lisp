(in-package :cl-repl)

(defun escape-name (name)
  (ppcre:regex-replace-all
    "\\+"
    (ppcre:regex-replace-all
      "\\*"
      name "\\\\*")
    "\\\\+"))

(defun list-regex (lst)
;   (format nil "(?<=\\b)(~{~a|~})(?=\\b)" lst))
  (format nil "((?<=\\s)|^|(?<=\\()|(?<=\\)))(~{~a|~})(?=\\b)" lst))

(destructuring-bind (functions specials)
  (loop :for sym :being :the :external-symbols :of :cl
        :when (handler-case (symbol-function sym) (error () nil))
              :collect (escape-name (string-downcase sym)) :into functions
        :when (special-operator-p sym)
              :collect (escape-name (string-downcase sym)) :into specials
        :finally (return (list functions specials)))
  (defvar *syntax-table*
    (list
     :string (list *string-syntax-color* "\".*?\"")
     :variable (list *variable-syntax-color* "([\\*])\\S+\\1")
     :constant (list *constant-syntax-color* "([\\+])\\S+\\1")
     :keyword (list *keyword-syntax-color* "((?<=\\s)|^):\\S+(?=\\b)")
     :definition (list *definition-syntax-color*
                       "((?<=defun)|(?<=defmacro)|(?<=defmethod)|(?<=defgeneric))\\s\\S+(?=\\b)")
     :lambda (list *lambda-syntax-color*
                   (list-regex '("&allow-other-keys" "&aux" "&body" "&environment" "&key" "&optional" "&rest" "&whole")))
     :special (list *special-syntax-color* (list-regex specials))
     :function (list  *function-syntax-color* (list-regex functions))
     :boolean (list *boolean-syntax-color* (list-regex '("nil" "t")))
     :normal (list *normal-syntax-color* "."))))

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
