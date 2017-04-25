#lang s-exp "../private/base.rkt"

(require "list.rkt")
(provide (all-defined-out))

; Y
; The Y-combinator
(def Y
  (fn (b)
    ((fn (f) (b (fn (x) ((f f) x))))
     (fn (f) (b (fn (x) ((f f) x)))))))

;; Y*
;; the Y-combinator, generalized for multiple argument functions
(def Y*
  (fn (b)
    ((fn (f) (b (fn args (apply (f f) args))))
     (fn (f) (b (fn args (apply (f f) args)))))))

; (partial *fun* *init-args* ...)
; Returns a new function with with init-args partially applied to fun
(def fn partial (fun . rest)
  (fn (x . args) (apply fun (append rest (append (list x) args)))))

; (compose *fun* *fun2*)
; Returns a new function which is the composition of the two, 
; returning the result of fun on the evaluation of fun2 and it's args. 
(def fn compose (fun fun2)
  (fn (x . args) (fun (apply fun2 (join x args)))))

; (fnlet *name* args body ...)
; A syntax sugaring for less verbose use of Y
; Allows lambda functions that can still self-refer
; uses the generalized Y-combinator
(def macro fnlet (name args body ...)
  (Y*
   (fn (name)
     (fn args
       body ...))))

; (identity v)
; Any -> Any
; The identity function. Given a value, returns that value. 
(def fn identity (v) v)
