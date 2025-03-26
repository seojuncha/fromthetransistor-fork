(load "package.lisp") 
(load "lexer.lisp")
(in-package :c-compiler)

(defun parsing (tokens)
  (format t ">>> start parsing....~%")
  ; entry point of parsing
  ; The final ast would be returned.
  (parse-translation-unit tokens))


;;; C Syntax 
;;; Based on ISO/IEC 9899:1999, Annex A Language syntax summary
;;;
;;; <AST types>
;;; ast-function-definition
;;;  :return-type :name :param :body
;;; ast-literal
;;; ast-identifier

(defstruct decl-specifier
  type 
  qualifier
  storage-class)

;;; <CFG> translation-unit: 
;;;   external-declaration
;;;   translation-unit external-declaration
(defun parse-translation-unit (tokens)
  (format t "check first token ~a~%" (car tokens))
  (format t "check token type(int) = ~a~%" (eq (token-type (car tokens)) :token-int))
  (format t "check token type(short) = ~a~%" (eq (token-type (car tokens)) :token-short))
  (parse-external-declaration tokens))

;;; <CFG> external-declaration:
;;;  function-definition
;;;  declaration
(defun parse-external-declaration (tokens)
  ; start with <declaration-specifiers> in both of <function-definition> and <declaration>
  (parse-function-definition tokens))

;;; <CFG> function-definition:
;;;   declaration-specifiers declarator declaration-list* compount-statement
;;;
;;; <AST> ast-function-definition
;;;  :return-type : <type-specifier>
;;;  :name : <declarator> -> <identifier>
;;;  :param : <parameter-type-list> -> <parameter-list> -> <parameter-declaration> -> <declaration-specifiers> <declarator>
;;;  :body : <compound-statement> -> <block-item-list> -> <block-item> -> <statement> -> <jump-statement> -> <expression> -> ... -> <primary-expression> -> <constant> -> <integer-constant>
;;;
;;; For example,
;;;   int main(void) { return 1; }
;;; 
;;; ast-function-definition
;;;   :return-type = 'int
;;;   :name = (ast-identifier "main")
;;;   :param = '()
;;;   :body = (ast-jump-statement :type 'return :expr (ast-literal 1))
(defun parse-function-definition (tokens)
  (multiple-value-bind (declspec tok-rest-1) (parse-declaration-specifiers tokens)
    (format t "[DEBUG] declspec: ~a~%" declspec)
    (format t "[DEBUG] token rest: ~a~%" tok-rest-1)
    (multiple-value-bind (ast-attr-function-name ast-attr-function-param tok-rest-2) (parse-declarator tok-rest-1)
      (format t "[DEBUG] function name: ~a~%" ast-attr-function-name)
      (format t "[DEBUG] function param: ~a~%" ast-attr-function-param)
      (format t "[DEBUG] token rest: ~a~%" tok-rest-2)
      (parse-compound-statement tok-rest-2))))

;; declaration-list
;;    declaration
;;    declaration-list declaration
(defun parse-declaration-list (tokens))

;;; <CFG> declaration-specifiers:
;;;   storage-class-specifier declaration-specifiers?
;;;   type-specifier declaration-specifiers?
;;;   type-qualifier declaration-specifiers?
;;;   function-specifier declaration-specifiers?
;;;
;;; For example,
;;;   int a
;;;   const int b
;;;   static const int c
(defun parse-declaration-specifiers (tokens)
  (let ((type nil)
        (qualifier nil)
        (storage-class nil)
        (rest tokens))
    (loop while (car rest) for tok = (token-type (car rest)) do
      (cond
        ((storage-class-specifier? tok) (setf storage-class tok) (setf rest (cdr rest)))
        ((type-specifier? tok) (setf type tok) (setf rest (cdr rest)))
        ((type-qualifier? tok) (setf qualifier tok) (setf rest (cdr rest)))
        (t (return))))
    (values (make-decl-specifier :type type :qualifier qualifier :storage-class storage-class) rest)))

(defun storage-class-specifier? (token-type)
  (member token-type '(:token-typedef :token-extern :token-static :token-auto :token-register)))

(defun type-specifier? (token-type)
  (member token-type '(:token-void :token-int :token-char)))

(defun type-qualifier? (token-type)
  (member token-type '(:token-const :token-volatile)))

(defun function-specifier? (token-type)
  (eq token-type :token-inline))

;;; <CFG> declarator:
;;;    pointer* direct-declarator
;;;
;;; NOTE: skip pointer now!
(defun parse-declarator (tokens)
  (parse-direct-declarator tokens))

;;; <CFG> direct-declarator:
;;;    identifier
;;;    direct-declarator "(" parameter-type-list ")"
;;;    ....others skip now....
;;;
;;; For example,
;;;   int main(void) { ... }
;;;      |=========|
;;;  <direct-declarator>
;;;
;;; (ast-identifier main)
;;; (ast-param '())
;;; 
;;; direct-declarator -> main(void)
;;; direct-declarator
;;;   - direct-declarator
;;;      - identifier -> (ast-identifier main)
;;;   - "(" parameter-type-list ")"
;;;      - '()
(defun parse-direct-declarator (tokens)
  (if (eq (token-type (car tokens)) :token-identifier)
    ; ast-identifier -> (ast-identifier main)
    ; tok-rest -> (void)
    (multiple-value-bind (ast-attr-function-name tok-rest-1) (parse-identifier tokens)
      (format t "[DEBUG] ast-identifier: ~a~%" ast-attr-function-name)
      (format t "[DEBUG] token rest: ~a~%" tok-rest-1)
      (if (eq (token-type (car tok-rest-1)) :token-open-paran)
        (multiple-value-bind (tok-dummy-1 tok-rest-2) (expect-token tok-rest-1 :token-open-paran)
          (multiple-value-bind (ast-attr-function-param tok-rest-3) (parse-parameter-type-list tok-rest-2)
            (format t "[DEBUG] ast-attr-function-param: ~a~%" ast-attr-function-param)
            (format t "[DEBUG] token rest: ~a~%" tok-rest-3)
            (multiple-value-bind (tok-dummy-2 tok-rest-4) (expect-token tok-rest-3 :token-close-paran)
              (values ast-attr-function-name ast-attr-function-param tok-rest-4))))))))

;; parameter-type-list:
;;    parameter-list
;;    parameter-list "," "..."
(defun parse-parameter-type-list (tokens)
  (parse-parameter-list tokens))


;; parameter-list:
;;    parameter-declartion
;;    parameter-list "," parameter-declartion
(defun parse-parameter-list (tokens)
  (parse-parameter-declaration tokens))

;; parameter-declaration:
;;    declaration-specifiers declarator
;;    declaration-specfiers abstract-declarator*
(defun parse-parameter-declaration (tokens)
  (multiple-value-bind (declspec tok-rest-1) (parse-declaration-specifiers tokens)
    (format t "[DEBUG] declspec: ~a~%" declspec)
    (format t "[DEBUG] token rest: ~a~%" tok-rest-1)
    (if (eq (decl-specifier-type declspec) :token-void)
      (values (list '()) tok-rest-1)  ; return empty parameter list
      (parse-declarator tok-rest-1))))

;; declaration:
;;    declaration-specifiers init-declarator-list* ";"
(defun parse-declaration (tokens))

;; init-declarator-list:
;;    init-declarator
;;    init-declarator-list "," init-declarator
(defun parse-init-declarator-list (tokens))

;; init-declarator:
;;    declarator
;;    declarator "=" initializer
(defun parse-init-declarator (tokens))


;; statement:
;;    labeled-statement
;;    compound-statement
;;    expression-statement
;;    selection-statement
;;    iteration-statement
;;    jump-statement
(defun parse-statement (tokens)
  (cond
    ((jump-statement? (token-type (car tokens)))
     (parse-jump-statement tokens))
    (t
     (format t "[ERROR] Not supported statement~%"))))

;; <CFG> compound-statement:
;;    "{" block-item-list* "}"
(defun parse-compound-statement (tokens)
  (multiple-value-bind (tok-dummy-1 tok-rest) (expect-token tokens :token-open-brace)
    (format t "[DEBUG] token rest: ~a~%" tok-rest)
    (parse-block-item-list tok-rest)))

;; <CFG> block-item-list
;;    block-item
;;    block-item-list block-item 
(defun parse-block-item-list (tokens)
  (parse-block-item tokens))

;; <CFG> block-item:
;;    declaration
;;    statement
(defun parse-block-item (tokens)
  (parse-statement tokens))

;; jump-statement:
;;    "goto" identifier ";"
;;    "continue" ";"
;;    "break" ";"
;;    "return" expression* ";"
(defun parse-jump-statement (tokens)
  ; NOTE: now support only return.
  (let ((tok-type (token-type (car tokens))))
    (cond
      ((eq tok-type :token-return)
        (multiple-value-bind (tok tok-rest) (expect-token tokens :token-return)
        (format t "[DEBUG] token rest: ~a~%" tok-rest)
        (parse-expression tok-rest)))
      ((eq tok-type :token-goto))
      ((or (eq (tok-type :token-break) (eq tok-type :token-continue))))
      (t
        (format t "[ERROR] Not jump statement~%")))))

(defun jump-statement? (tok-type)
  (or
    (eq tok-type :token-return)
    (eq tok-type :token-goto)
    (eq tok-type :token-continue)
    (eq tok-type :token-break)))

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
      ((eq (token-type tok) :token-identifier)
       (list 'ast-identifier (token-lexeme tok)))
      ((eq (token-type tok) :token-number)
       (list 'ast-literal (token-lexeme tok)))
      (t
       (format t "Invalid token~%")))))


;; identifier:
;;    identifier-nondigit
;;    identifier identifier-nondigit
;;    identifier digit
(defun parse-identifier (tokens)
  (multiple-value-bind (tok-id tok-rest) (expect-token tokens :token-identifier)
    (values (list 'ast-identifier (token-lexeme tok-id)) tok-rest)))


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
  (token-type (car tokens)))

;;; This function returns the rest of tokens
;;; if the first token is matched with the expected token type.
;;; Q. What if the type is not matched? How can I stop parsing?
(defun expect-token (tokens expect-token-type)
  (let ((tok (car tokens)))
    (if (eq (token-type tok) expect-token-type)
      (values tok (cdr tokens))
      (format t "[ERROR] Unexpected token: ~a~%" tok))))
