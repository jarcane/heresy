#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

; (one? n)
; Returns True if number is 1.
(def fn one? (n)
  (= n 1))