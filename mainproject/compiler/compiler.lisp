(load "package.lisp") 
(load "lexer.lisp")
(load "parser.lisp")
(load "codegen.lisp")

(in-package :c-compiler)

(defparameter *c-file-path* ())
(defun parse-arguments (args)
  (format t "command line arguments: ~a~%" args)
  (loop until (endp args) do
    (push (pop args) *c-file-path*)))

(defun run ()
  (parse-arguments (cdr sb-ext:*posix-argv*))
  (when (= 0 (length *c-file-path*))
    (format t "no file~%")
    (sb-ext:quit :unix-status 1))
  (loop until (endp *c-file-path*) do
    (let ((parse-file (pop *c-file-path*)))
      (with-open-file (instream parse-file)
        (let ((contents (make-string (file-length instream))))
          (read-sequence contents instream)
          (format t "read source...~%~a~%" contents)
          (let ((tokens (tokenize contents)))
            (format t "tokens: ~a~%" tokens)
            (parsing tokens)))))))

(run)