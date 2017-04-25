#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import "theory.rkt")
(provide (all-defined-out))

(describe Some (contains Null))
(describe None)

(def fn some (v)
  (Some (list v)))

(def fn is-some? (opt)
  (is-a? Some opt))

(def fn is-none? (opt)
  (equal? opt None))

(def fn maybe-bind (opt fn)
  (select
    ((is-none? opt) None)
    (else (fn (opt 'contains)))))

(def fn yield (v)
  (some v))

(def fn get-some (opt)
  (maybe-bind opt identity))

(def fn maybe-map (fn opt)
  (select
    ((is-none? opt) opt)
    (else (some (fn (opt 'contains))))))

(def fn maybe-filter (pred? opt)
  (select
    ((is-none? opt) opt)
    ((pred? (opt 'contains)) opt)
    (else None)))

(def macroset maybe-do (= <-)
  ((maybe-do (exp ...)) (exp ...))
  ((maybe-do (name = val) exp ...)
   (maybe-bind (some val) (fn (name) (maybe-do exp ...))))
  ((maybe-do (name <- val) exp ...)
   (maybe-bind val (fn (name) (maybe-do exp ...)))))