(load "package.lisp") 

(in-package :c-compiler)

(defconstant +token-1-open-paran+ #\()
(defconstant +token-1-close-paran+ #\))
(defconstant +token-1-open-curly-bracket+ #\{)
(defconstant +token-1-close-curly-bracket+ #\})
(defconstant +token-1-equal+ #\=)
(defconstant +token-1-semicolon+ #\;)
(defconstant +token-newline+ #\Newline)

(defparameter *token-type* '(:open-paran
                             :close-paran
                             :open-curly-bracket
                             :close-curly-bracket
                             :equal
                             :semicolon
                             :eof))

(defstruct token
  token-type
  lexeme
  literal
  line)

(defparameter *tokens* '())


(defun scanning (source)
  (format t ">>> start scanning.... ~d characters.~%" (length source))
  (let ((start 0) (current 0) (line 1) c)
    (loop while (< current (length source)) do
      (setq c (char source current))
      (format t "[~d] ~a~%" current c)
      (cond 
        ((char= c +token-1-open-paran+)
          (push (make-token :token-type :open-paran :line line) *tokens*))
        ((char= c +token-1-close-paran+)
          (push (make-token :token-type :close-paran :line line) *tokens*))
        ((char= c +token-newline+)
          (incf line))
        (t (cond 
              ((digit-char-p c) (format t "numeric~%") )
              ((alpha-char-p c) (format t "alphabet~%")))))
      (incf current)))
  (push (make-token :token-type :eof) *tokens*)
  *tokens*)