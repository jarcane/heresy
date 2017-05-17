#lang s-exp "../private/base.rkt"

(import "./things.rkt")
(import "./pipes.rkt")
(provide (all-defined-out))

; State
; A handy empty object for use as a State store
(describe State)

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