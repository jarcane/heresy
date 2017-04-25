#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import "theory.rkt")
(provide (all-defined-out))

;; The Maybe type family
; Some
; Contains a value
(describe Some (contains Null))

; None
; Represents no result
(describe None)

; (some v)
; Any -> Some
; Constructor for Some. Returns a Some containing v.
(def fn some (v)
  (Some (list v)))

; (is-some? opt)
; Maybe -> Boolean
; Returns true if opt is a Some
(def fn is-some? (opt)
  (is-a? Some opt))

; (is-none? opt)
; Maybe -> Boolean
; Returns true if opt is None.
(def fn is-none? (opt)
  (equal? opt None))

; (maybe? v)
; Any -> Boolean
; Returns true if v is a Maybe.
(def fn maybe? (opt)
  (or (is-some? opt)
      (is-none? opt)))

; (maybe-bind opt fn)
; Maybe Fn(Any -> Maybe) -> Maybe
; The bind (>>=) operator for Some/None.
; Returns None if opt is None, or fn applied to the contents of Some.
(def fn maybe-bind (opt fn)
  (select
    ((is-none? opt) None)
    (else (fn (opt 'contains)))))

; (yield v)
; Any -> Some
; Returns v wrapped in Some
(def fn yield (v)
  (some v))

; (get-some opt)
; Maybe -> Any
; If opt is Some(v), returns v, else if None, returns None.
(def fn get-some (opt)
  (maybe-bind opt identity))

; (maybe-map fn opt)
; Fn(Any -> Any) Maybe -> Maybe
; If opt is Some(v), returns Some(fn v), else if None, Returns None
(def fn maybe-map (fn opt)
  (select
    ((is-none? opt) opt)
    (else (some (fn (opt 'contains))))))

; (maybe-filter pred? opt)
; If opt is None, returns None. If opt is Some(v), returns Some(v) if (pred? v) is true,
; or None if it is false
(def fn maybe-filter (pred? opt)
  (select
    ((is-none? opt) opt)
    ((pred? (opt 'contains)) opt)
    (else None)))

; (maybe-do ...)
; Do notation micro-DSL for Maybe.
; For each line but the last, the following two forms are allowed:
; (name <- val) - binds Maybe val to name.
; (name = val) - binds Some(val) to name
; Subsequent val expressions have the previous named vals in scope.
; The last line must be a normal expression, most useful if it is a calculation of previous
; bound values. A bare expression will return its result, use (yield ...) to return a Some.
(def macroset maybe-do (= <- yield)
  ((maybe-do (exp ...)) (exp ...))
  ((maybe-do (name = val) exp ...)
   (maybe-bind (some val) (fn (name) (maybe-do exp ...))))
  ((maybe-do (name <- val) exp ...)
   (maybe-bind val (fn (name) (maybe-do exp ...)))))