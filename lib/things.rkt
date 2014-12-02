#lang s-exp "../private/base.rkt"

(require "list.rkt")

(provide (all-defined-out))

; (describe *thing* (*field* *value*) ...)
; Declare a new kind of Thing, with the given fields and default values.
(def macro describe (name (field value) ...)
  (def name
    (thing '((field value) ...))))

(def fn thing (lst)
  (fn args*
      (let ([alst lst])
        (select
         ((null? args*) alst)
         ((symbol? (head args*)) (tail (assoc (head args*) alst)))
         ((list? (head args*)) 
          (let recur ([al alst]
                      [pat (head args*)]
                      [c 1])
            (select
             ((null? pat) (thing al))
             ((eq? (head pat) '*) (recur alst (tail pat) (+ 1 c)))
             (else (recur (subst (head (index c al))
                                 (join (head pat) Null)
                                 al)
                     (tail pat)
                     (+ 1 c))))))
         (else (error "Thing expected a symbol or a pattern"))))))
