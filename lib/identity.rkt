#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import "monadology.rkt")

; A container thing 
(describe Identity (state Null))

; The type constructor for the Identity monad
(def fn id (v)
  (Identity (list v)))

; The bind (>>=) operator for the Identity monad
(def fn id-bind (act fn)
  (fn (act 'state)))

; The guard function for identity. Essentially meaningless, but necessary for the definition
(def fn id-guard (test)
  (if test then (id Null) else Null))

; An instance of monad-do for Identity
(def macroset id-do
  ((id-do e ...)
   (monad-do (id-bind id id-guard) e ...)))