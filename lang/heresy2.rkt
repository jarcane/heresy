#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         )

;;; Heresy 2 syntax parser

;; Utility parsers



;; Primitives

; Null
; Null

; Boolean
; True | False

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


;; Operators

; BinOp
; Expr Op Expr
; Op = + | - | * | / | ^ | < | > | = | =$ | & | and | or

; UnaryOp
; Op Expr
; Op = not

; FunctionAppl
; Identifier \( Expr+(,) \)