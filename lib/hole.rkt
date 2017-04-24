#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import rkt racket)
(import rkt racket/async-channel)
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
  (def chan (rkt:make-async-channel))
  (def thr
    (rkt:thread
     (fn ()
       (do loop with val
           (def signal (rkt:async-channel-get chan))
           (select case (signal 'type)
                   ((get) (do
                            (rkt:async-channel-put chan cry)
                            (carry cry)))
                   ((reset) (do
                              (rkt:async-channel-put chan (signal 'new-val))
                              (carry (signal 'new-val))))
                   ((update) (rkt:with-handlers ((rkt:exn:fail?
                                                  (fn (e)
                                                    (rkt:async-channel-put chan e)
                                                    (carry cry))))
                                                (def result (apply (signal 'update-fn) cry (signal 'args)))
                                                (rkt:async-channel-put chan result)
                                                (carry result)))
                   (else (carry cry)))))))
  (Hole (list thr chan)))

; (hole? *v*)
; Any -> Boolean
; Returns #t if v is a hole.
(def fn hole? (v)
  (and (is-a? Hole v)
       (rkt:async-channel? (v 'channel))
       (rkt:thread? (v 'thread))))

; (deref *hol*)
; Hole -> Any
; Returns the current value store in hol.
(def fn deref (hol)
  (rkt:async-channel-put (hol 'channel) (Signal `(get)))
  (rkt:async-channel-get (hol 'channel)))

; (reset *hol* *new-val*)
; Hole Any -> Hole
; Resets the current value of hol to new-val
(def fn reset (hol new-val)
  (rkt:async-channel-put (hol 'channel) (Signal `(reset ,new-val)))
  (rkt:async-channel-get (hol 'channel))
  hol)

; (update *hol* *fn* . *args* ...)
; Hole Fn . Args -> Hole
; Updates the current value of hol by applying fn with the current value as first arg, and args
; as the remaining arguments
(def fn update (hol fn . args)
  (rkt:async-channel-put (hol 'channel) (Signal `(update * ,fn ,args)))
  (def result (rkt:async-channel-get (hol 'channel)))
  (if (rkt:exn:fail? result) then
      (rkt:raise result) else
      hol))

; (reset-thing *hol* *sym* *val* ...)
; Hole Field Any ... -> Hole
; Expects a Thing stored in hol, and resets the value in the thing field defined by sym to val
(def macro reset-thing (hol (field val) ...)
  (update hol (fn (t)
                (thing extends t (field val) ...))))