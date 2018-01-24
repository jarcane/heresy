#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import "monadology.rkt")
(import rkt racket)
(import rkt racket/async-channel)
(import rkt rackjure/utils)
(provide (all-defined-out))

; Hole
; Core data type for holes. Not especially useful on its own, except as a reference for is-a?.
(describe Hole
          (box Null))

; (hole *val*)
; Any -> Hole
; Returns a hole containing val. Holes carry a single value, with a limited set of atomic
; operations.  
(def fn hole (val)
  (Hole (list (rkt:box val))))

; (hole? *v*)
; Any -> Boolean
; Returns #t if v is a hole.
(def fn hole? (v)
  (and (is-a? Hole v)
       (rkt:box? (v 'box))))

; (deref *hol*)
; Hole -> Any
; Returns the current value store in hol.
(def fn deref (hol)
  (rkt:unbox (hol 'box)))

; (reset *hol* *new-val*)
; Hole Any -> Hole
; Resets the current value of hol to new-val
(def fn reset (hol new-val)
  (rkt:set-box! (hol 'box) new-val)
  hol)

; (update *hol* *fn* . *args* ...)
; Hole Fn . Args -> Hole
; Updates the current value of hol by applying fn with the current value as first arg, and args
; as the remaining arguments
(def fn update (hol fn . args)
  (apply rkt:box-swap! (hol 'box) fn args)
  hol)

; (reset-thing *hol* *sym* *val* ...)
; Hole Field Any ... -> Hole
; Expects a Thing stored in hol, and resets the value in the thing field defined by sym to val
(def macro reset-thing (hol (field val) ...)
  (update hol (fn (t)
                (thing extends t (field val) ...))))