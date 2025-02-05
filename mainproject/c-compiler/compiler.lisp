#!/usr/bin/sbcl --script

(load "package.lisp") 
(load "scanner.lisp")
(load "parser.lisp")
(load "codegen.lisp")

(in-package :c-compiler)

(defparameter *c-file-path* ())
(defun parse-arguments (args)
  (format t "command line arguments: ~a~%" args)
  (format t "length: ~d~%" (length args))
  (loop until (endp args) do
    (format t "here~%")
    (push (pop args) *c-file-path*)))

(defun run ()
  (parse-arguments (cdr sb-ext:*posix-argv*))
  (when (= 0 (length *c-file-path*))
    (format t "no file~%")
    (sb-ext:quit :unix-status 1))
  (loop until (endp *c-file-path*) do
    ; read line-by-line
    (pop *c-file-path*)
    ; scanning input: source line
    ; scanning output: list of tokens
    (scanning "return 32;")
    (parsing)
    (codegen)))

(run)