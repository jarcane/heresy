#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import rkt racket)
(provide (all-defined-out))

; Signal
; A helper data type for signal passing to hole threads
(describe Signal
          (type Null)
          (new-val Null)
          (update-fn (fn (x) x))
          (args Null))

; Hole
; Core data type for holes. Not especially useful on its own, except as a reference for is-a?.
(describe Hole
          (thread Null)
          (channel Null))

; (hole *val*)
; Any -> Hole
; Returns a hole containing val. Holes are tiny in-memory databases that carry a single value,
; and communicate by passing messages to and from the thread.  
(def fn hole (val)
  (def chan (rkt:make-channel))
  (def thr
    (rkt:thread
     (fn ()
       (do loop with val
           (def signal (rkt:channel-get chan))
           (select case (signal 'type)
                   ((get) (do
                            (rkt:channel-put chan cry)
                            (carry cry)))
                   ((reset) (do
                              (rkt:channel-put chan (signal 'new-val))
                              (carry (signal 'new-val))))
                   ((update) (rkt:with-handlers ((rkt:exn:fail?
                                                  (fn (e)
                                                    (rkt:channel-put chan e)
                                                    (carry cry))))
                                                (def result (apply (signal 'update-fn) cry (signal 'args)))
                                                (rkt:channel-put chan result)
                                                (carry result)))
                   (else (carry cry)))))))
  (Hole (list thr chan)))

; (deref *hol*)
; Hole -> Any
; Returns the current value store in hol.
(def fn deref (hol)
  (rkt:channel-put (hol 'channel) (Signal `(get)))
  (rkt:channel-get (hol 'channel)))

; (reset *hol* *new-val*)
; Hole Any -> Hole
; Resets the current value of hol to new-val
(def fn reset (hol new-val)
  (rkt:channel-put (hol 'channel) (Signal `(reset ,new-val)))
  hol)

; (update *hol* *fn* . *args* ...)
; Hole Fn . Args -> Hole
; Updates the current value of hol by applying fn with the current value as first arg, and args
; as the remaining arguments
(def fn update (hol fn . args)
  (rkt:channel-put (hol 'channel) (Signal `(update * ,fn ,args)))
  (def result (rkt:channel-get (hol 'channel)))
  (if (rkt:exn:fail? result) then
      (rkt:raise result) else
      hol))

; (reset-thing *hol* *sym* *val*)
; Hole Field Any -> Hole
; Expects a Thing stored in hol, and resets the value in the thing field defined by sym to val
(def macro reset-thing (hol (field val) ...)
  (update hol (fn (t)
                (thing extends t (field val) ...))))


(def foo (hole 1))
(reset foo 2)
(deref foo)
(update foo + 5)
(deref foo)
(reset foo (thing (foo 1)))
((deref foo))
(reset-thing foo (foo 2))
((deref foo))