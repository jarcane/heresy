#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

; (:> *initial-value* *fn1* ...)
; The forward pipe operator. Given an initial value and a list of one-argument functions
; applys functions in order from left to right and returns the result
(def fn :> (initial-value . fns)
  (for (f in fns with initial-value)
    (carry (f cry))))

; (f> *fn* *args* ...)
; For currying fns for :>. Returns a function that takes initial-value and applies it as the first argument of fn
(def macro f> (f args ...)
  (fn (x)
      (f x args ...)))

; (l> *fn* *args* ...)
; The inverse of f>, returns a function that takes a value and applies it as the last argument of fn
(def macro l> (f args ...)
  (fn (x)
      (f args ... x)))

; (-> *value* *fns* ...)
; The first-argument threading macro. Takes value, and threads it in turn as the first argument of the following functions
(def macro -> (iv (f args ...) ...)
  (:> iv
      (f> f args ...)
      ...))

; (->> *value* *fns* ...)
; The last-argument version of ->. Takes a value, and threads it in turn as the last argument of successive functions
(def macro ->> (iv (f args ...) ...)
  (:> iv
      (l> f args ...)
      ...))