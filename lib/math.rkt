#lang s-exp "../private/base.rkt"

(import rkt racket)
(provide (all-defined-out))

(def pi 3.141592653589793)
(def e  2.718281828459045)

; (abs n)
; returns the absolute value of n
(def fn abs (n)
  (if (< n 0) then (* n -1) else n))

; (one? n)
; Returns True if number is 1.
(def fn one? (n)
  (= n 1))

; (even? n)
; returns True if n is even
(def fn even? (n)
  (select
   ((not (rkt:integer? n)) 
    (error 'even? "expected integer"))
   ((zero? n) True)
   ((< n 0) (odd? (inc n)))
   (else (odd? (dec n)))))

; (odd? n)
; return True if n is odd
(def fn odd? (n)
  (select
   ((not (rkt:integer? n)) 
    (error 'odd? "expected integer"))
   ((zero? n) False)
   ((< n 0) (even? (inc n)))
   (else (even? (dec n)))))

; (sgn n)
; Returns the "sign" of n, -1 if neg, 0 if zero?, or 1 if positive
(def fn sgn (n)
  (select 
   ((< n 0) -1)
   ((> n 0) 1)
   (else 0)))

; (inc n)
; increments n by 1
(def fn inc (n)
  (+ 1 n))

; (dec n)
; decrements n by 1
(def fn dec (n)
  (- n 1))

; (exp n)
; Returns the value of e^n
(def fn exp (n)
  (^ e n))

; (sin x)
; Sine of x
(def fn sin (x)
  (rkt:real-part 
   (/ (- (^ e (* 0+1i x)) (^ e (* 0-1i x)))
     0+2i)))

; (cos x)
; Cosine of x
(def fn cos (x)
  (rkt:real-part 
   (/ (+ (^ e (* 0+1i x)) (^ e (* 0-1i x)))
     2)))

; (tan x)
; Tangent of x
(def fn tan (x)
  (/ (sin x)
     (cos x)))