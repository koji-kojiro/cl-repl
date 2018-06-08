(in-package :cl-repl)

(defvar *keymap*)
(defvar *keymaps* (make-hash-table :test 'equal))
(defvar *rl-default-keymap* (rl:get-keymap))

(defmacro define-keymap (name (&optional parent) &body bindings)
  (let ((keymap (gensym)))
    `(let ((,keymap (rl:copy-keymap (or (find-keymap ,parent) *rl-default-keymap*))))
       (loop :for (key func) :in ',bindings
             :when (stringp key)
             :do (rl:bind-keyseq key (eval func) :keymap ,keymap)
             :when (characterp key)
             :do (rl:bind-key key (eval func) :keymap ,keymap))
       (setf (gethash ,name *keymaps*) ,keymap))))

(defun unbind-key (args key)
  (declare (ignore args key)))

(defun find-keymap (name)
  (gethash name *keymaps*))

(define-keymap "default" ()
  ("\\C-r" #'unbind-key)
  ("\\C-s" #'unbind-key)
  ("\\C-l" (lambda (&rest args)
             (declare (ignore args))
             (invoke-magic "%cls"))))

(defun set-keymap (name)
  (let ((keymap (find-keymap name)))
    (unless (null keymap)
      (setf *keymap* name)
      (rl:set-keymap keymap))))

(set-keymap "default")
