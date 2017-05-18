#lang s-exp "../private/base.rkt"

(import "things.rkt")

; A container thing 
(describe Identity (state Null))

; The type constructor for the Identity monad
(def fn id (v)
  (Identity (list v)))

; The bind (>>=) operator for the Identity monad
(def fn id-bind (act fn)
  (fn (act 'state)))

; A simple DSL for handling imperative operations
(def macroset do> (:= return)
  ((_ (return exp ...)) (exp ...))
  ((_ (name := val) exp ...)
   (id-bind (id val) (fn (name) (do> exp ...))))
  ((_ (exp0 ...) exp1 ...)
   (id-bind (id (exp0 ...)) (fn (_) (do> exp1 ...)))))