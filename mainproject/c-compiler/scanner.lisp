(load "package.lisp") 

(in-package :c-compiler)

(defconstant +token-open-paran+ #\()
(defconstant +token-close-paran+ #\))
(defconstant +token-open-curly-bracket+ #\{)
(defconstant +token-close-curly-bracket+ #\})
(defconstant +token-equal+ #\=)
(defconstant +token-semicolon+ #\;)
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

(defun scanning (source)
  (format t ">>> start scanning.... ~d characters.~%" (length source))
  (loop with line = 1 and index = 0
        while (< index (length source)) do
    (let ((start index)
          (offset 0)
          (character nil))
      (setq character (char source index))
      (format t "[line:~d][index:~d] ~a~%" line index character)

      (cond 
        ((char= character +token-open-paran+)
          (push (make-token :token-type :open-paran :line line) *tokens*) (incf index))
        ((char= character +token-close-paran+)
          (push (make-token :token-type :close-paran :line line) *tokens*) (incf index))
        ((char= character +token-open-curly-bracket+)
          (push (make-token :token-type :open-curly-bracket :line line) *tokens*) (incf index))
        ((char= character +token-close-curly-bracket+)
          (push (make-token :token-type :close-curly-bracket :line line) *tokens*) (incf index))
        ((char= character +token-semicolon+)
          (push (make-token :token-type :semicolon :line line) *tokens*) (incf index))

        ((char= character +token-space+)
          (incf index))
        ((char= character +token-newline+)
          (incf line) (incf index))
        (t (cond
              ((digit-char-p character) 
                (format t "numeric~%") (incf index))
              ((alpha-char-p character)
                (format t "alphabet~%")
                (incf index (identifier source start offset)))
              (t (format t "unkown: ~s~%" character) (incf index))))
          )))
  (push (make-token :token-type :eof) *tokens*)
  *tokens*)

(defun identifier (source start offset)
  (loop while (or (digit-char-p (char source (+ start offset))) (alpha-char-p (char source (+ start offset)))) do
                          (incf offset))
  (format t "offset: ~d~%" offset)

  (let ((substring nil))
    (setq substring (subseq source start (+ start offset)))
    (format t "substring: ~s~%" substring)
    ; compare wheter it is keyword.
  ) offset)