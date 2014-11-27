#lang s-exp "../private/base.rkt"

(import rkt racket)
(provide (all-defined-out))

(def pi 3.141592653589793)
(def e  2.718281828459045)

; (one? n)
; Returns True if number is 1.
(def fn one? (n)
  (= n 1))

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