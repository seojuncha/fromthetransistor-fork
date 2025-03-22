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

(defparameter *token-type* '(:token-open-paran            ; (
                             :token-close-paran           ; )
                             :token-open-curly-bracket    ; {
                             :token-close-curly-bracket   ; }
                             :token-comma     ; ,
                             :token-dot       ; .
                             :token-minus     ; -
                             :token-plus      ; +
                             :token-slash     ; /
                             :token-star      ; *
                             :token-bang      ; !
                             :token-mod       ; %
                             :token-semicolon

                             :token-equal    ; =
                             :token-equal-equal  ; ==
                             :token-greater  ; >
                             :token-greater-equal ; >=
                             :token-less   ; <
                             :token-less-equal ; <=
                             :token-minus-equal  ; -=
                             :token-plus-equal   ; +=
                             :token-star-equal   ; *=
                             :token-slash-equal  ; /=
                             :token-mod-equal    ; %=

                             :token-identifier
                             :token-string
                             :token-number  ; only digit number now.

                             ; keywords
                             :token-short
                             :token-int
                             :token-long
                             :token-float
                             :token-double
                             :token-char
                             :token-signed
                             :token-unsigned

                             :token-return
                             :token-goto
                             :token-break
                             :token-continue
                             
                             :token-if
                             :token-else
                             :token-else-if
                             :token-switch
                             :token-case

                             :token-for
                             :token-while
                             :token-do

                             :token-static
                             :token-exstern
                             :token-volatile

                             :token-const
                             :token-enum
                             :token-sizeof
                             :token-struct
                             :token-union
                             :token-typedef
                             :token-void

                             ; eof
                             :token-eof))

(defstruct token
  token-type
  lexeme
  literal
  line)

(defparameter *keywords* (make-hash-table :test 'equal))
;;; data type
(setf (gethash "short" *keywords*) :token-short)
(setf (gethash "int" *keywords*) :token-int)
(setf (gethash "long" *keywords*) :token-long)
(setf (gethash "float" *keywords*) :token-float)
(setf (gethash "double" *keywords*) :token-double)
(setf (gethash "char" *keywords*) :token-char)
(setf (gethash "signed" *keywords*) :token-signed)
(setf (gethash "unsigned" *keywords*) :token-unsigned)
;;; flow control
(setf (gethash "return" *keywords*) :token-return)
(setf (gethash "goto" *keywords*) :token-goto)
(setf (gethash "break" *keywords*) :token-break)
(setf (gethash "continue" *keywords*) :token-continue)
;;; condition
(setf (gethash "if" *keywords*) :token-if)
(setf (gethash "else" *keywords*) :token-else)
(setf (gethash "else if" *keywords*) :token-else-if)
(setf (gethash "case" *keywords*) :token-case)
(setf (gethash "switch" *keywords*) :token-switch)
;;; iteration
(setf (gethash "for" *keywords*) :token-for)
(setf (gethash "while" *keywords*) :token-while)
(setf (gethash "do" *keywords*) :token-do)
;;; storage
(setf (gethash "static" *keywords*) :token-static)
(setf (gethash "extern" *keywords*) :token-extern)
(setf (gethash "volatile" *keywords*) :token-volatile)
;;; others
(setf (gethash "const" *keywords*) :token-const)
(setf (gethash "enum" *keywords*) :token-enum)
(setf (gethash "sizeof" *keywords*) :token-sizeof)
(setf (gethash "struct" *keywords*) :token-struct)
(setf (gethash "union" *keywords*) :token-union)
(setf (gethash "typedef" *keywords*) :token-typedef)
(setf (gethash "void" *keywords*) :token-void)

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
          (push (make-token :token-type :token-open-paran :lexeme character :line line) *tokens*) (incf index))
        ((char= character +token-close-paran+)
          (push (make-token :token-type :token-close-paran :lexeme character :line line) *tokens*) (incf index))
        ((char= character +token-open-curly-bracket+)
          (push (make-token :token-type :token-open-curly-bracket :lexeme character :line line) *tokens*) (incf index))
        ((char= character +token-close-curly-bracket+)
          (push (make-token :token-type :token-close-curly-bracket :lexeme character :line line) *tokens*) (incf index))
        ((char= character +token-equal+)
          (push (make-token :token-type :token-equal :lexeme character :line line) *tokens*) (incf index))
        ((char= character +token-semicolon+)
          (push (make-token :token-type :token-semicolon :lexeme character :line line) *tokens*) (incf index))

        ((char= character +token-space+)
          (incf index))
        ((char= character +token-newline+)
          (incf line) (incf index))

        (t (cond
              ((digit-char-p character) 
                (format t "numeric~%")
                (incf index (digit-number source start offset line)))
              ((alpha-char-p character)
                (format t "alphabet~%")
                (incf index (identifier source start offset line)))
              (t (format t "unknown: ~s~%" character) (incf index))))
          )))
  (push (make-token :token-type :eof) *tokens*)
  *tokens*)

(defun digit-number (source start offset line)
  (loop while (or (digit-char-p (char source (+ start offset)))) do
                          (incf offset))
  (format t "offset: ~d~%" offset)
  (let ((substring nil))
    (setq substring (subseq source start (+ start offset)))
    (format t "number substring: ~s~%" substring)
    (push (make-token :token-type :token-number :lexeme substring :line line) *tokens*))
  offset)

(defun identifier (source start offset line)
  (loop while (or 
                (digit-char-p (char source (+ start offset))) 
                (alpha-char-p (char source (+ start offset)))
                (char= #\_ (char source (+ start offset)))) do
    (incf offset))
  (format t "offset: ~d~%" offset)

  (let ((substring nil))
    (setq substring (subseq source start (+ start offset)))
    (format t "identifier substring: ~s~%" substring)
    (multiple-value-bind (value present) (gethash substring *keywords*)
      (if present
        (progn 
          (format t ">>> \"~a\" is keyword!~%" substring)
          (push (make-token :token-type value :lexeme substring :line line) *tokens*))
        (progn
          (format t ">>> \"~a\" is identifier!~%" substring)
          (push (make-token :token-type :token-identifier :lexeme substring :line line) *tokens*)))))
  offset)