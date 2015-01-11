#lang heresy

(import rkt racket/base)
(require rackunit syntax/parse/define (rkt:for-syntax racket/base))

(rkt:begin-for-syntax
  (define-syntax-class chk-clause
    [pattern (~and stx [actual:expr expected:expr])
             #:with chk (syntax/loc #'stx (check-equal? actual expected))]))
(define-simple-macro (chk clause:chk-clause ...)
  (rkt:begin clause.chk ...))

(test-case "Y"
  (def fact
    (Y (fn (f)
         (fn (n)
           (select
            [(zero? n) 1]
            [else (* n (f (dec n)))])))))
  (chk [(fact 0) 1]
       [(fact 1) 1]
       [(fact 2) 2]
       [(fact 3) 6]
       [(fact 4) 24]
       [(fact 5) 120]
       [(fact 6) 720]))

(test-case "Y*"
  (def fib
    (Y* (fn (f)
          (fn (f0 f1 n)
            (select
             [(zero? n) f0]
             [(one? n)  f1]
             [else (+ (f f0 f1 (- n 2)) (f f0 f1 (- n 1)))])))))
  (chk [(fib 0 1 0) 0] ; Fibonacci Numbers
       [(fib 0 1 1) 1]
       [(fib 0 1 2) 1]
       [(fib 0 1 3) 2]
       [(fib 0 1 4) 3]
       [(fib 0 1 5) 5]
       [(fib 0 1 6) 8]
       [(fib 0 1 7) 13]
       [(fib 0 1 8) 21]
       [(fib 0 1 9) 34]
       [(fib 0 1 10) 55]
       [(fib 0 1 11) 89]
       [(fib 0 1 12) 144]
       [(fib 0 1 13) 233]
       [(fib 0 1 14) 377]
       [(fib 0 1 15) 610]
       [(fib 0 1 16) 987]
       [(fib 2 1 0) 2] ; Lucas Numbers
       [(fib 2 1 1) 1]
       [(fib 2 1 2) 3]
       [(fib 2 1 3) 4]
       [(fib 2 1 4) 7]
       [(fib 2 1 5) 11]
       [(fib 2 1 6) 18]
       [(fib 2 1 7) 29]
       [(fib 2 1 8) 47]
       [(fib 2 1 9) 76]
       [(fib 2 1 10) 123]
       [(fib 2 1 11) 199]
       [(fib 2 1 12) 322]))

(test-case "fnlet"
  (def fib
    (fnlet f (f0 f1 n)
      (select
       [(zero? n) f0]
       [(one? n)  f1]
       [else (+ (f f0 f1 (- n 2)) (f f0 f1 (- n 1)))])))
  (chk [(fib 0 1 0) 0] ; Fibonacci Numbers
       [(fib 0 1 16) 987]
       [(fib 2 1 0) 2] ; Lucas Numbers
       [(fib 2 1 12) 322]))

