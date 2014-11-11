#lang racket

;; Heresy - a BASIC-flavored Lisp dialect
; Copyright (C) 2014 John S. Berry III
; Licensed with the LGPL v.3.0

;; Provides

;; Support Functions

; REPL

;                                                    
;                                                    
;                                                    
;   ;;;;;;       ;;       ;;;;      ;;;;      ;;;;   
;     ;   ;;    ;  ;    ;;    ;;     ;;     ;;    ;; 
;     ;   ;;    ;  ;    ;;    ;;     ;;     ;;    ;; 
;     ;   ;;  ;;    ;;    ;          ;;     ;;       
;     ;;;;    ;;    ;;     ;;        ;;     ;;       
;     ;;;;    ;;    ;;     ;;        ;;     ;;       
;     ;   ;;  ;;;;;;;;       ;       ;;     ;;       
;     ;   ;;  ;;    ;;  ;;    ;;     ;;     ;;    ;; 
;     ;   ;;  ;;    ;;  ;;    ;;     ;;     ;;    ;; 
;   ;;;;;;    ;;    ;;    ;;;;      ;;;;      ;;;;   
;                                                    
;                                                    
;                                                    

;; Declarations

; DIM name ['(length)]
; Defines a variable name with null contents '(), or length #f values
(define-syntax dim
  (syntax-rules ()
    [(_ name) (define name '())]
    [(_ name '(length)) 
     (define name (for/list ([i (in-range length)])
                    #f))]))

; LET

; DEF

;; Flow Control

; IF

; FOR

; DO

; SELECT

;; I/O

; PRINT

; INPUT

;; Math

; # (infix operator)

; RND

;; Miscellaneous

; REM


;                                  
;                                  
;                                  
;                                  
;   ;;;      ;;;;;   ;;; ;  ;;;;;  
;    ;         ;    ;   ;;   ;   ; 
;    ;         ;    ;        ;   ; 
;    ;         ;     ;;;;    ;   ; 
;    ;         ;         ;   ;;;;  
;    ;   ;     ;         ;   ;     
;    ;   ;     ;    ;;   ;   ;     
;   ;;;;;;   ;;;;;  ; ;;;   ;;;    
;                                  
;                                  
;                                  
;                                  

;; Lists

; CONS (JOIN?)

; HEAD

; TAIL

;; Predicates

; LIST?

; ATOM? 

; NULL?

; ZERO?

; EQ?

; =?

;; Functions/Macros

; LAMBDA (FN?)

; MACRO 

; EVAL

; QUOTE

;; Boolean

; AND

; OR

; NOT

; True

; False