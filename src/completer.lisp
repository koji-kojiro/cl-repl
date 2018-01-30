(in-package :cl-repl)

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
      (return-from completer '("")))
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

