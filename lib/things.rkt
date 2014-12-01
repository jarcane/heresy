#lang s-exp "../private/base.rkt"

(import "list.rkt")

; (describe *thing* (*field* *value*) ...)
; Declare a new kind of Thing, with the given fields and default values.
(def macro describe (name (field . value) ...)
  (def name
    (fn args*
        (let ([alst '((field . value) ...)])
          (select
           ((symbol? (head args*)) (tail (assoc (head args*) alst)))
           ((list? (head args*)) (rem Do Stuff Here))
           (else (error "Thing expected a symbol or a pattern")))))))

(describe dave (weight 100 200 300)
          (status 'fat))

(dave 'weight)
(dave 'status)