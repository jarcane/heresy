#lang s-exp "../private/base.rkt"

(import "./things.rkt")
(provide (all-defined-out))

; (:> *initial-value* *fn1* ...)
; The forward pipe operator. Given an initial value and a list of one-argument functions
; applys functions in order from left to right and returns the result
(def fn :> (initial-value . fns)
  (for (f in fns with initial-value)
    (carry (f cry))))

; State
; A handy empty object for use as a State store
(describe State (new (fn () State)))

; (:= [(*names* ...)] *var* *value*)
; State -> State
; Binds a new value to var in State. Can overwrite previous values
; If *names* are provided, they are bound in the scope for use in value clause
(def macroset :=
  [(:= (name ...) var value)
   (fn (s)
     (thing extends s
            (var (let ([name (s (quote name))] ...)
                   value))))]
  [(:= var value)
   (fn (s) (thing extends s (var value)))])

; (:_ [(*names* ...)] *fn* *args* ...)
; State -> State
; Executes fn with args, optionally binding names to the local scope for use in by fn, then ignores their return value and returns State
(def macroset :_ 
  [(:_ (name ...) f args ...)
   (fn (s)
       (let ([name (s (quote name))] ...)
         (f args ...)
         s))]
  [(:_ f args ...)
   (fn (s)
       (f args ...)
       s)])

; (return *value*)
; State -> value
; Returns the given value from State
; Always use last
(def macro return (name)
  (fn (s) (s (quote name))))

; (do> *actions* ...)
; A useful shortcut for using State with :>
(def fn do> fns
  (apply :> (join State fns)))

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