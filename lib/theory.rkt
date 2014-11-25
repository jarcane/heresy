#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

; Y
; The Y-combinator
(def Y
  (fn (b)
    ((fn (f) (b (fn (x) ((f f) x))))
     (fn (f) (b (fn (x) ((f f) x)))))))