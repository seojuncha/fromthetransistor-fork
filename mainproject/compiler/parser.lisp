(load "package.lisp") 
(load "scanner.lisp")
(in-package :c-compiler)

(defun parsing (tokens)
  (format t ">>> start parsing....~%")
  ; entry point of parsing
  ; The final ast would be returned.
  (parse-translation-unit tokens))


;;;; C Syntax 
;;;; Based on ISO/IEC 9899:1999, Annex A Language syntax summary

(defstruct decl-specifier
  specifier
  qualifier
  storage-class)

;; translation-unit: 
;;   external-declaration
;;   translation-unit external-declaration
(defun parse-translation-unit (tokens)
  (format t "check first token ~a~%" (car tokens))
  (format t "check token type(int) = ~a~%" (eq (token-token-type (car tokens)) :token-int))
  (format t "check token type(short) = ~a~%" (eq (token-token-type (car tokens)) :token-short))
  (parse-external-declaration tokens))

;; external-declaration:
;;  function-definition
;;  declaration
(defun parse-external-declaration (tokens)
  ; start with <declaration-specifiers> in both of <function-definition> and <declaration>
  (parse-function-definition tokens))

;; function-definition:
;;   declaration-specifiers declarator decaration-list* compount-statement
(defun parse-function-definition (tokens)
  (multiple-value-bind (declspec rest-tokens) (parse-declaration-specifiers tokens)
    (format t "[DEBUG] declspec: ~a~%" declspec)
    (format t "[DEBUG] rest: ~a~%" rest-tokens)
    (multiple-value-bind (tok rest-tokens-2) (parse-declarator rest-tokens)
      (parse-compound-statement rest-tokens-2))))

;; declaration:
;;   declaration-specifiers init-declarator-list* ";"
(defun parse-declaration (tokens))


;; declaration-specifiers:
;;   storage-class-specifier declaration-specifiers?
;;   type-specifier declaration-specifiers?
;;   type-qualifier declaration-specifiers?
;;   function-specifier declaration-specifiers?
(defun parse-declaration-specifiers (tokens)
  (let ((specifier nil)
        (qualifier nil)
        (storage-class nil)
        (rest tokens))
    (loop while (car rest) for tok = (token-token-type (car rest)) do
      (cond
        ((storage-class-specifier? tok) (setf storage-class tok) (setf rest (cdr rest)))
        ((type-specifier? tok) (setf specifier tok) (setf rest (cdr rest)))
        ((type-qualifier? tok) (setf qualifier tok) (setf rest (cdr rest)))
        (t (return))))
    (values (make-decl-specifier :specifier specifier :qualifier qualifier :storage-class storage-class) rest)))

(defun storage-class-specifier? (token-type)
  (member token-type '(:token-typedef :token-extern :token-static :token-auto :token-register)))

(defun type-specifier? (token-type)
  (member token-type '(:token-void :token-int :token-char)))

(defun type-qualifier? (token-type)
  (member token-type '(:token-const :token-volatile)))

(defun function-specifier? (token-type)
  (eq token-type :token-inline))

;; declarator:
;;    pointer* direct-declarator
(defun parse-declarator (tokens)
  ; skip pointer now!
  (parse-direct-declarator tokens))


;; direct-declarator:
;;    identifier
;;    "(" declarator ")"
;;    direct-declarator "(" parameter-type-list ")"
;;    direct-declarator "(" identifier-list_opt ")"
;;    ....
(defun parse-direct-declarator (tokens)
  (multiple-value-bind (tok rest-tokens) (expect-token tokens :token-identifier)
    (format t "[DEBUG] tok: ~a~%" tok)
    (format t "[DEBUG] rest: ~a~%" rest-tokens)
    (if tok
      (values (list 'ast-identifier :name (token-lexeme tok)) rest-tokens)
      (format t "[ERROR] is not idenfier~%"))))


;; parameter-type-list:
;;    parameter-list
;;    parameter-list "," "..."


;; parameter-list:
;;    parameter-declartion
;;    parameter-list "," parameter-declartion

;; parameter-declaration:
;;    declaration-specifiers declarator
;;    declaration-specfiers abstract-declarator_opt


;; compound-statement:
;;    "{" block-item-list*"}"
(defun parse-compound-statement (tokens)
  ())


;; block-item-list
;;    block-item
;;    block-item-list block-item 

;; block-item:
;;    declaration
;;    statement

;; statement:
;;    labeled-statement
;;    compound-statement
;;    expression-statement
;;    selection-statement
;;    iteration-statement
;;    jump-statement

;; jump-statement:
;;    "goto" identifier ";"
;;    "continue" ";"
;;    "break" ";"
;;    "return" expression* ";"
(defun parse-jump-statement (tokens)
  (let ((rest-token (expect-token tokens :token-return)))
    (let ((expr (parse-expression rest-token)))
      (expect-token rest-token :token-semicolon))))


;; expression:
;;    assignment-expression
;;    expression "," assignment-expression
(defun parse-expression (tokens))

;; assignment-expression:
;;    conditional-expression
;;    unary-expression assignment-operator assignment-expression
(defun parse-assignment-expression (tokens))


;; unary-expression:
;;    postfix-expression
;;    "++" unary-expression
;;    "--" unary-expression
;;    unary-operator cast-expression
;;    "sizeof" unary-expression
;;    "sizeof" "(" type-name ")"
(defun parse-unary-expression (tokens))

;; postfix-expression
;;    primary-expression
;;    postfix-expression "[" expression "]"
;;    ....(later!)
(defun parse-postfix-expression (tokens))


;; primary-expression:
;;    identifier
;;    constant
;;    string-literal
;;    "(" expression ")"
(defun parse-primary-expression (tokens)
  (let ((tok (car tokens)))
    (cond
      ((eq (token-token-type tok) :token-identifier)
       (list 'ast-identifier (token-lexeme tok)))
      ((eq (token-token-type tok) :token-number)
       (list 'ast-number (token-lexeme tok)))
      (t
       (format t "Invalid token~%")))))


;; identifier:
;;    identifier-nondigit
;;    identifier identifier-nondigit
;;    identifier digit

;; identifier-nondigit:
;;    nondigit
;;    universal-character-name

;; nondigit: one of
;;    "_" [a-zA-Z]

;; digit: one of
;;    [0-9]
;;    "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"


;; constant:
;;    integer-constant
;;    floating-constant
;;    enumeration-constant
;;    character-constant
; (defun parse-constant (tokens)
;   ; NOTE: only deal with integer now.
;   (list 'ast-int-const  ))


;;;; Utility Functions
(defun next-token (tokens)
  (token-token-type (car tokens)))

;;; This function returns the rest of tokens
;;; if the first token is matched with the expected token type.
;;; Q. What if the type is not matched? How can I stop parsing?
(defun expect-token (tokens expect-token-type)
  (let ((tok (car tokens)))
    (if (eq (token-token-type tok) expect-token-type)
      (values tok (cdr tokens))
      (format t "[ERROR] Unexpected token: ~a~%" tok))))
