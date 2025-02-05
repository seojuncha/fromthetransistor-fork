(load "package.lisp") 

(in-package :c-compiler)

(defun scanning (source-line)
  ;; read one character.
  ;; 
  (format t "start scanning....[~a]~%" source-line)
  (let ((start 0))
    (loop for offset from 0 to (length source-line) do
      ; update start when a token is found.
      (setq start (+ start offset))
      )))