#lang s-exp "../private/base.rkt"

(require "list.rkt")
(provide (all-defined-out))

; Y
; The Y-combinator
(def Y
  (fn (b)
    ((fn (f) (b (fn (x) ((f f) x))))
     (fn (f) (b (fn (x) ((f f) x)))))))

; (partial *fun* *init-args* ...)
; Returns a new function with with init-args partially applied to fun
(def fn partial (fun . rest)
  (fn (x . args) (apply fun (append rest (append (list x) args)))))