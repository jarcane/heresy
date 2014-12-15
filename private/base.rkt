#lang racket

;; Heresy - a BASIC-flavored Lisp dialect
; Copyright (C) 2014 John S. Berry III
; Licensed with the LGPL v.3.0

;; Requires
(require racket/stxparam
         (only-in racket/base [case rkt:case]))

;; Provides
(provide (all-defined-out)
         ; required
         #%module-begin
         #%top-interaction
         #%app #%datum #%top
         
         ; From Racket
         + - / * = < >
         list? null? zero? eq?
         symbol?
         and or not else
         quote quasiquote 
         unquote unquote-splicing
         let list apply
         require provide all-defined-out
         error
         (for-syntax ...)
         (rename-out (cons join)
                     (car head)
                     (cdr tail)
                     (eval run)
                     ))

;; Declarations

; (IMPORT name)
; (IMPORT RKT name)
; requires the given file, importing it's names
(define-syntax import
  (syntax-rules (rkt)
    [(_ rkt name) (require (prefix-in rkt: name))]
    [(_ name) (require name)]))
(define-syntax-parameter rkt (λ (stx) (error "rkt is an import keyword only")))

; (LET ((name value) ...) ...)
; Defines a variable in the local context. 
; provided by Racket

; (DEF name contents)
; (DEF FN name (args) body)
; (DEF MACRO name (pattern-vars) pattern)
; Defines new variables and functions (with help from FN)
(define-syntax def
  (syntax-rules (macro fn)
    [(_ macro name (args ... . rest) body0 bodyn ...) 
     (define-syntax-rule (name args ... . rest) body0 bodyn ...)]
    [(_ fn name (args ... . rest) body0 bodyn ...) 
     (define (name args ... . rest) body0 bodyn ...)]
    [(_ name contents) (define name contents)]))

; DEF literals
(define-syntax-parameter macro 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "macro must be used with def")))
;(define-syntax-parameter fn 
;  (lambda (stx)
;    (raise-syntax-error (syntax-e stx) 
;                        "fn must be used with def; use lambda for anonymous functions")))

; (FN (args) body ...)
; The anonymous function 
(define-syntax fn
  (syntax-rules ()
    [(_ (args ... . rest) body ...) (lambda (args ... . rest) body ...)]
    [(_) (error 'fn "Missing syntax")]))

;; Flow Control

; (IF test THEN do1 ELSE do2)
; (IF test THEN do)
; Basic conditional execution block
(define-syntax if
  (syntax-rules (then else)
    [(_ test then do1 else do2) (cond [test do1] [else do2])]
    [(_ test then do) (when test do)]))

(define-syntax-parameter then 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "then can only be used inside if")))

; (FOR (var OVER list) body... [CARRY value] [BREAK [value]]
; Iterates over list in val, CARRYing value assigned from accumulator to next loop
; CRY contains the accumulator, initialized to '()
; thanks to chandler in #racket for the assistance
(define-syntax-parameter carry 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "carry can only be used inside loops")))
(define-syntax-parameter cry 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "cry can only be used inside loops")))
(define-syntax-parameter in 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "in can only be used inside for")))
(define-syntax-parameter with 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "with can only be used inside for")))
(define-syntax for
  (syntax-rules (in with)
    [(_ (var in lst with x) body ...) 
     (let/ec break-k
       (syntax-parameterize 
        ((break (syntax-rules () 
                  [(_ ret) (break-k ret)]
                  [(_) (break-k)])))
        (let loop ((cry-v x)
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
                          (cdr l)))])))))]
    [(_ (var in lst) body ...) 
     (let/ec break-k
       (syntax-parameterize 
        ((break (syntax-rules () 
                  [(_ ret) (break-k ret)]
                  [(_) (break-k)])))
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
  (syntax-rules (loop with)
    [(_ loop with x body ...) (let/ec break-k
                                (syntax-parameterize 
                                 ((break (syntax-rules () 
                                           [(_ ret) (break-k ret)]
                                           [(_) (break-k)]))) 
                                 (let loop ([cry-v x])
                                   (syntax-parameterize
                                    ((cry (make-rename-transformer #'cry-v)))
                                    (loop
                                     (call/ec
                                      (lambda (k)
                                        (syntax-parameterize
                                         ((carry (make-rename-transformer #'k)))
                                         body ...)
                                        cry-v)))))))]
    [(_ loop body ...) (let/ec break-k
                         (syntax-parameterize 
                          ((break (syntax-rules () 
                                    [(_ ret) (break-k ret)]
                                    [(_) (break-k)]))) 
                          (let loop ([cry-v '()])
                            (syntax-parameterize
                             ((cry (make-rename-transformer #'cry-v)))
                             (loop
                              (call/ec
                               (lambda (k)
                                 (syntax-parameterize
                                  ((carry (make-rename-transformer #'k)))
                                  body ...)
                                 cry-v)))))))]
    [(_ body ...) (begin body ...)]))

; (SELECT [test op1] ... [ELSE opn])
; (SELECT CASE test [test-result op1] ... [else opn])
; Multiple conditional block: COND-style, or CASE style with CASE.
(define-syntax select
  (syntax-rules (case)   
    [(select case expr ((result1 ...) op1) ... (else opn)) 
     (rkt:case expr [(result1 ...) op1] ... (else opn))]
    [(select (test op1) ... (else opn)) 
     (cond [test op1] ... (else opn))]))

(define-syntax-parameter case
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "case can only be used with select")))

;; I/O

; PRINT LIT -> print
; PRINT & -> display
; PRINT -> displayln
(define-syntax print
  (syntax-rules (lit &)
    [(_ lit datum) (write datum)]
    [(_ & datum) (display datum)]
    [(_ datum) (displayln datum)]
    [(_) (newline)]))

(define-syntax-parameter lit 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "lit can only be used with print")))

; ? (shortcut for print)
(define-syntax ?
  (syntax-rules ()
    [(_ a ...) (print a ...)]))

; INPUT -> read-line (current-input-port)
; INPUT STX -> read
(define-syntax input
  (syntax-rules (stx)
    [(_ stx) (read)]
    [(_ str) (begin (display str) (read-line))]
    [(_) (read-line)]))

(define-syntax-parameter stx 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "stx can only be used with input")))

;; Strings

; (=$ str ...)
; Compares strings for equality
(define-syntax =$
  (syntax-id-rules ()
    [(_ a b ...) (string=? a b ...)]
    [=$ string=?]))

; & str ... (string concat)
; Concats strings
(define-syntax &
  (syntax-id-rules ()
    [(& a b ...) (string-append a b ...)]
    [& string-append]))

; LIST$ list 
; Converts a string into a list of single character strings
(define (list$ l)
  (map string (string->list l)))

; (str$ *num*) 
; Converts a number into a string
(define (str$ n)
  (number->string n))

;; Math

; ^
(define-syntax ^
  (syntax-rules ()
    [(_ a b) (expt a b)]))

; (mod x y)
; returns the modulo of x over y
(define-syntax mod
  (syntax-rules ()
    [(_ x y) (modulo x y)]))

; ! (infix operator)
(define-syntax !
  (syntax-rules ()
    [(! a fun b) (fun a b)]))

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

; (range x to y [step s])
; Generates a list of numbers from x to y by step
(define (gen-range x y (step 1) (lst '()))
  (cond
    [(= step 0) (error 'range "infinite loop detected")]
    [(or (and (> x y) (> step -1))
         (and (< x y) (< step 1))) lst]
    [(= x y) (cons x lst)]
    [else (cons x (gen-range (+ x step) y step lst))]))

(define-syntax range
  (syntax-rules (to step)
    [(_ x to y step s) (gen-range x y s)]
    [(_ x to y) (gen-range x y)]
    [(_) (error 'range "malformed range")]))

(define-syntax-parameter to 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "to can only be used in range")))
(define-syntax-parameter step 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "step can only be used in range")))

;; Meta Functions

; RUN
; Provided by Racket eval

; QUOTE
; Provided by Racket quote

; REM
(define-syntax rem
  (syntax-rules ()
    [(rem a ...) (void)]))

;; Boolean

; True
(define-syntax True (syntax-id-rules (True) (True #t)))

; False
(define-syntax False (syntax-id-rules (False) (False #f)))

; Null
(define-syntax Null (syntax-id-rules (Null) (Null '())))