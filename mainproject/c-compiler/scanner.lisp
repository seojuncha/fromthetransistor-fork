(load "package.lisp") 

(in-package :c-compiler)

(defconstant +token-1-open-paran+ #\()
(defconstant +token-1-close-paran+ #\))
(defconstant +token-1-open-curly-bracket+ #\{)
(defconstant +token-1-close-curly-bracket+ #\})
(defconstant +token-1-equal+ #\=)
(defconstant +token-1-semicolon+ #\;)
(defconstant +token-newline+ #\newline)
(defconstant +token-space+ #\space)

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

(defun identifier (source start offset)
  (loop while (or (digit-char-p (char source (+ start offset))) (alpha-char-p (char source (+ start offset)))) do
                          (incf offset))
  (format t "offset: ~d~%" offset)

  (let ((substring nil))
    (setq substring (subseq source start (+ start offset)))
    (format t "substring: ~a~%" substring)
    ; compare wheter it is keyword.
  ) offset)


(defun scanning (source)
  (format t ">>> start scanning.... ~d characters.~%" (length source))
  (loop with line = 1 and index = 0
        while (< index (length source)) do
    (let ((start index)
          (offset 0)
          (chracter nil))
      (setq chracter (char source index))
      (incf index)
      (format t "[line:~d][index:~d] ~a~%" line index chracter)

      (cond 
        ((char= chracter +token-1-open-paran+)
          (push (make-token :token-type :open-paran :line line) *tokens*))
        ((char= chracter +token-1-close-paran+)
          (push (make-token :token-type :close-paran :line line) *tokens*))
        ((char= chracter +token-newline+)
          (incf line))
        (t (cond
              ((digit-char-p chracter) 
                (format t "numeric~%"))
              ((alpha-char-p chracter)
                (format t "alphabet~%")
                (incf index (identifier source start offset)))))
          )))
  (push (make-token :token-type :eof) *tokens*)
  *tokens*)