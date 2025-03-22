(load "package.lisp") 
(in-package :c-compiler)

;;;; Utils
(defun next-token ())

(defun expect-token (tokens expect-token-type))

;;;; C Syntax

;; translation-unit: 
;;   external-declaration
;;   translation-unit external declaration
(defun parse-translation-unit (tokens))

;; external-declaration:
;;  function-definition
;;  declaration
(defun parse-external-declaratino (tokens))

;; function-definition:
;;   declaration-specifiers declarator decaration-list_opt compount-statement
(defun parse-function-definition (tokens))

;; declaration:
;;   declaration-specifiers init-declarator-list_opt ";"
(defun parse-declaration (tokens))


;; declaration-specifiers:
;;   storage-class-specifier declaration-specifiers_opt
;;   type-specifier declaration-specifiers_opt
;;   type-qualifier declaration-specifiers_opt
;;   function-specifier declaration-specifiers_opt
(defun parse-declaration-specifiers (tokens))


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

;; jum-statement:
;;    "goto" identifier ";"
;;    "continue" ";"
;;    "break" ";"
;;    "return" expression* ";"


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



(defun parsing (tokens)
  (format t "start parsing....~%")
  (parse-translation-unit tokens))
