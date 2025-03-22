(load "package.lisp") 
(load "scanner.lisp")
(in-package :c-compiler)

(defun parsing (tokens)
  (format t ">>> start parsing....~%")
  (parse-translation-unit tokens))

;;;; Utility Functions
(defun next-token (tokens)
  (car tokens))

(defun expect-token (tokens expect-token-type))

;;;; C Syntax

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
(defun parse-external-declaration (tokens))

;; function-definition:
;;   declaration-specifiers declarator decaration-list_opt compount-statement
(defun parse-function-definition (tokens))

;; declaration:
;;   declaration-specifiers init-declarator-list* ";"
(defun parse-declaration (tokens))


;; declaration-specifiers:
;;   storage-class-specifier declaration-specifiers*
;;   type-specifier declaration-specifiers*
;;   type-qualifier declaration-specifiers*
;;   function-specifier declaration-specifiers*
(defun parse-declaration-specifiers (tokens))


(defun is-storage-class-specifier (token)
  (member token '(:token-typedef :token-extern :token-static :token-auto :token-register)))

(defun is-type-specifier? (token)
  (member token '(:token-void :token-int :token-char)))

(defun is-type-qualifier? (token)
  (member token '(:token-const :token-volatile)))

(defun is-function-specifier? (token)
  (eq token :token-inline))

;; declarator:
;;    pointer_opt direct-declarator
(defun parse-declarator (tokens))


;; direct-declarator:
;;    identifier
;;    "(" declarator ")"
;;    direct-declarator "(" parameter-type-list ")"
;;    direct-declarator "(" identifier-list_opt ")"
;;    ....
(defun parse-direct-declarator (tokens))


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
(defun parse-compound-statement (tokens))


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
(defun parse-jump-statement (tokens))


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
(defun parse-primary-expression (tokens))


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
(defun parse-constant (tokens))



