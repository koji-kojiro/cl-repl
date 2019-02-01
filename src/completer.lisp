(in-package :cl-repl)

(defun get-package-for-search (text)
  (let ((pos))
    (cond
      ((setf pos (search "::" text))
       (list  (subseq text  (+ pos 2)) (subseq text 0 pos) nil))
      ((setf pos (position #\: text))
       (if (zerop pos)
         (list text nil t)
         (list (subseq text (1+ pos)) (subseq text 0 pos) t)))
      (t (list text nil  t)))))

(defun list-external-symbols (sym-name pkg-name)
  (declare (ignore sym-name))
  (loop :for sym :being :the :external-symbols :of (find-package pkg-name)
        :collect (format nil "~(~a:~a~)" pkg-name sym)))

(defun list-internal-symbols (sym-name pkg-name)
  (declare (ignore sym-name))
  (loop :for sym :being :the :symbols :of (find-package pkg-name)
        :collect (format nil "~(~a::~a~)" pkg-name sym)))

(defun list-symbols-and-packages (sym-name)
  (declare (ignore sym-name))
  (concatenate 'list
    (loop :for pkg :in (list-all-packages)
          :append (loop :for name :in (package-nicknames pkg)
                        :collect (format nil "~(~a:~)" name))
          :collect (format nil "~(~a:~)" (package-name pkg)))
    (loop :for sym :being :the :symbols :of *package*
          :collect (string-downcase sym))
    (loop :for kw :being :the :symbols :of (find-package "KEYWORD")
          :collect (format nil ":~(~a~)" kw))))

(defun select-completions (text items)
  (setf items
    (loop :for item :in items
          :when (string-starts-with item text)
                :collect item))
  (unless (cdr items)
    (setf rl:*completion-append-character*
      (if (string-ends-with (car items) ":") #\nul #\space))
    (return-from select-completions items))
  (cons
    (subseq (car items) 0
      (loop :for item :in (cdr items)
            :minimize (or (mismatch (car items) item) (length item))))
    items))

(defun completer (text start end)
  (declare (ignore start end))
  (when (string-equal text "")
    (return-from completer nil))
  (destructuring-bind (sym-name pkg-name external-p)
    (get-package-for-search (string-upcase text))
    (when (and pkg-name (not (find-package pkg-name)))
      (return-from completer nil))
    (select-completions
      (string-downcase text) 
      (cond
        ((zerop (length pkg-name)) (list-symbols-and-packages sym-name))
        (external-p (list-external-symbols sym-name pkg-name))
        (t (list-internal-symbols sym-name pkg-name))))))
