#lang s-exp "../private/base.rkt"

(import "things.rkt")
(import rkt racket)
(provide (all-defined-out))

; Signal
; A helper data type for signal passing to hole threads
(describe Signal
          (type Null)
          (new-val Null)
          (thread-to-send (rkt:current-thread))
          (update-fn (fn (x) x))
          (args Null))

; (hole *val*)
; Any -> Hole
; Returns a hole containing val. Holes are tiny in-memory databases that carry a single value,
; and communicate by passing messages to and from the thread.  
(def fn hole (val)
  (rkt:thread
   (fn ()
     (do loop with val
       (def signal (rkt:thread-receive))
       (select case (signal 'type)
         ((get) (do
                  (rkt:thread-send (signal 'thread-to-send) cry)
                  (carry cry)))
         ((reset) (do
                    (rkt:thread-send (signal 'thread-to-send) (signal 'new-val))
                    (carry (signal 'new-val))))
         ((update) (rkt:with-handlers ((rkt:exn:fail?
                                        (fn (e)
                                            (rkt:thread-send (signal 'thread-to-send) e)
                                            (carry cry))))
                      (def result (apply (signal 'update-fn) cry (signal 'args)))
                      (rkt:thread-send (signal 'thread-to-send) result)
                      (carry result)))
         (else (carry cry)))))))

; (deref *hol*)
; Hole -> Any
; Returns the current value store in hol.
(def fn deref (hol)
  (rkt:thread-send hol (Signal `(get * ,(rkt:current-thread))))
  (rkt:thread-receive))

; (reset *hol* *new-val*)
; Hole Any -> Hole
; Resets the current value of hol to new-val
(def fn reset (hol new-val)
  (rkt:thread-send hol (Signal `(reset ,new-val)))
  hol)

; (update *hol* *fn* . *args* ...)
; Hole Fn . Args -> Hole
; Updates the current value of hol by applying fn with the current value as first arg, and args
; as the remaining arguments
(def fn update (hol fn . args)
  (rkt:thread-send hol (Signal `(update * ,(rkt:current-thread) ,fn ,args)))
  (def result (rkt:thread-receive))
  (if (rkt:exn:fail? result) then
      (rkt:raise result) else
      hol))
