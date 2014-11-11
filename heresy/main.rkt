#lang racket

;; Heresy - a BASIC-flavored Lisp dialect
; Copyright (C) 2014 John S. Berry III
; Licensed with the LGPL v.3.0

;; Provides
(provide (all-defined-out)
         + - / *
         and or not
         let
         (rename-out (lambda fn)
                     (cons join)
                     (car head)
                     (cdr tail)))

;; Support Functions/macros

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

; (LET ([name value] ...) ...)
; Defines a variable in the local context. 
; *Provided by Racket*

; (DEF name contents)
; (DEF FN name (args) body)
; Defines new variables and functions (with help from FN)
(define-syntax def
  (syntax-rules (fn)
    [(_ name contents) (define name contents)]
    [(_ fn name (args) body) (define (name args) body)]))

;; Flow Control

; (IF test THEN do1 ELSE do2)
; (IF test THEN do)
; Basic conditional execution block
(define-syntax if
  (syntax-rules (then else)
    [(_ test then do1 else do2) (cond [test do1] [else do2])]
    [(_ test then do) (when test do)]))

; FOR


; DO

; (SELECT [test op1] ... [ELSE opn])
; (SELECT CASE test [test-result op1] ... [else opn])
; Multiple conditional block: COND-style, or CASE style with CASE.
(define-syntax select
  (syntax-rules (case)   
    [(select case body ...) (select-case body ...)]
    [(select body ...) (select-cond body ...)]))

(define-syntax select-cond
  (syntax-rules (else)
    [(select-cond (test op1) ... (else opn)) 
     (cond [test op1] ... (else opn))]))

(define-syntax select-case
  (syntax-rules (else)
    [(select-case expr ((result1 ...) op1) ... (else opn))
     (case expr [(result1 ...) op1] ... (else opn))]))

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

; JOIN a b
; Provided by Racket cons

; HEAD list
; Provided by Racket car

; TAIL list
; Provided by Racket cdr

;; Predicates

; LIST?

; ATOM? 

; NULL?

; ZERO?

; EQ?

; =

;; Functions/Macros

; FN
; Provided by Racket lambda

; MACRO 

; EVAL

; QUOTE

;; Boolean

; True

; False