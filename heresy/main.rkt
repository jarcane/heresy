#lang racket

;; Heresy - a BASIC-flavored Lisp dialect
; Copyright (C) 2014 John S. Berry III
; Licensed with the LGPL v.3.0

;; Requires
(require racket/stxparam)

;; Provides
(provide (all-defined-out)
         + - / * =
         list? null? zero? eq?
         and or not
         let
         (rename-out (lambda fn)
                     (cons join)
                     (car head)
                     (cdr tail)))

;; Support Functions/macros

; REPL

;; Declarations

; (LET ([name value] ...) ...)
; Defines a variable in the local context. 
; *Provided by Racket*

; (DEF name contents)
; (DEF FN name (args) body)
; Defines new variables and functions (with help from FN)
(define-syntax def
  (syntax-rules (fn)
    [(_ fn name (args) body) (define (name args) body)]
    [(_ name contents) (define name contents)]))

;; Flow Control

; (IF test THEN do1 ELSE do2)
; (IF test THEN do)
; Basic conditional execution block
(define-syntax if
  (syntax-rules (then else)
    [(_ test then do1 else do2) (cond [test do1] [else do2])]
    [(_ test then do) (when test do)]))

; (FOR (var OVER list) body... [CARRY value] [BREAK [value]]
; Iterates over list in val, CARRYing value assigned from accumulator to next loop
; CRY contains the accumulator, initialized to '()
; thanks to chandler in #racket for the assistance
(define-syntax-parameter carry 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "carry can only be used inside for")))
(define-syntax-parameter cry 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "cry can only be used inside for")))
(define-syntax for
  (syntax-rules (in)
    [(_ (var in lst) body ...) 
     (let/ec break-k
       (syntax-parameterize 
        ((break (syntax-rules () [(_) (break-k)])))
        (let loop ((cry-v '())
                   (l lst))
          (syntax-parameterize
           ([cry (make-rename-transformer #'cry-v)])
           (cond [(null? l) cry-v]
                 [else (let ([var (car l)])
                         (loop
                          (call/ec
                           (lambda (k)
                             (syntax-parameterize
                              ([carry (make-rename-transformer #'k)])
                              body ...)
                             cry-v))
                          (cdr l)))])))))]))

; (DO body ...)
; (DO LOOP body ... [BREAK])
; executes a block of code, looping with LOOP until it encounters a BREAK
(define-syntax-parameter break
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "break can only be used inside loops")))

(define-syntax do
  (syntax-rules (loop)
    [(_ loop body ...) (let/ec break-k
                         (syntax-parameterize 
                          ((break (syntax-rules () [(_) (break-k)]))) 
                          (let loop () body ... (loop))))]
    [(_ body ...) (begin body ...)]))

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

; PRINT -> print

; PRINT$ -> display+sugar

; WRITE -> write

; INPUT -> read-line (current-input-port)

; READ -> read

;; Strings

; & str ... (string concat)
; Concats strings
(define-syntax &
  (syntax-rules ()
    [(& a ...) (string-append a ...)]))

; LIST$ list 
; Converts a string into a list of single character strings
(define (list$ l)
  (map string (string->list l)))

;; Math

; ^
(define-syntax ^
  (syntax-rules ()
    [(_ a b) (expt a b)]))

; ! (infix operator)
(define-syntax !
  (syntax-rules ()
    [(! a fun b ...) (fun a b ...)]))

; RND

;; Miscellaneous

; REM
(define-syntax rem
  (syntax-rules ()
    [(rem a ...) (void)]))

;; Lists

; JOIN a b
; Provided by Racket cons

; HEAD list
; Provided by Racket car

; TAIL list
; Provided by Racket cdr

;; Predicates
; Provided by racket
; LIST? l 
; NULL? l
; ZERO? n
; EQ? a b ...
; = a b ...

; ATOM? a
; Tests a given item to see if it is an atom (ie. not a list)
(define (atom? a)
  (and (not (pair? a)) (not (null? a))))

(define (lat? l)
  (cond 
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]))

;; Functions/Macros

; FN
; Provided by Racket lambda

; MACRO 

; EVAL

; QUOTE

;; Boolean

; True

; False