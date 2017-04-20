#lang heresy

(import rkt racket)
(import rkt racket/exn)

(describe Signal
          (type Null)
          (new-val Null)
          (thread-to-send (rkt:current-thread))
          (update-fn (fn (x) x))
          (args Null))

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

(def fn deref (a)
  (rkt:thread-send a (Signal `(get)))
  (rkt:thread-receive))

(def fn reset (a new-val)
  (rkt:thread-send a (Signal `(reset ,new-val)))
  (rkt:thread-receive))

(def fn update (a fn . args)
  (rkt:thread-send a (Signal `(update * * ,fn ,args)))
  (def result (rkt:thread-receive))
  (if (rkt:exn:fail? result) then
      (rkt:raise result) else
      result))
