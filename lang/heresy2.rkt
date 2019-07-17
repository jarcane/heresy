#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         )

;;; Heresy 2 syntax parser

;; Utility parsers



;; Primitives

; Null
; "Null"

; Boolean
; "True" | "False"

; String
; \" CHAR+ \"

; Integer
; (-) NUM+

; Float
; (-) NUM+ \. NUM+

; Exponential
; (-) Float \e [-|+] Integer

; Identifier
; ValidInit ValidChar+ (ValidSymbol)
; ValidInit = ALPHA
; ValidChar = ALPHA | NUM
; ValidSymbol = $ | # | ? 

; Symbol
; \: Identifier


; Value
; Null | Boolean | String | Integer | Float | Exponential | Identifier | Symbol

;; Operators

; BinOp
; Expr Op Expr
; Op = + | - | * | / | ^ | < | > | = | =$ | & | and | or

; UnaryOp
; Op Expr
; Op = not | carry

; FunctionAppl
; Identifier \( Expr+(,) \)

; If
; "if" Expr "then" Expr "else" Expr "end"

; For
; "for" Identifier "in" Expr ("with" Expr) Expr "end"

; Fn
; "fn" \( (Identifier+(,)) \) Expr "end"

; Block
; "begin" [ Expr \n ] "end"
 
; Expr
; ( \( ) [ BinOp | UnaryOp | FunctionAppl | Identifier | Block | Value ] ( \) )


;; Statements

; Def
; "def" Identifier \= Expr

; DefFn
; "def" "fn" Identifier \( (Identifier+(,)) \) Expr "end"

;; Misc

; Comment
; [ "rem" | \' ] Any+ \n