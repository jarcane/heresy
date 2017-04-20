#lang heresy

(import rkt racket)

(describe Signal
          (type Null)
          (new-val Null)
          (thread-to-send (rkt:current-thread))
          (update-fn (fn (x) x)))

(def fn atom (val)
  (rkt:thread
   (fn ()
       (do loop with val
         (def signal (rkt:thread-receive))
         (select case (signal 'type)
            ((get) (do
                     (rkt:thread-send (signal 'thread-to-send) cry)
                     (carry cry)))
            ((reset) (carry (signal 'new-val)))
            ((update) (carry ((signal 'update-fn) cry)))
            (else (carry cry)))))))

(def fn deref (a)
  (rkt:thread-send a (Signal `(get)))
  (rkt:thread-receive))

(def fn reset (a new-val)
  (rkt:thread-send a (Signal `(reset ,new-val))))

(def fn update (a fn)
  (rkt:thread-send a (Signal `(update * * ,fn))))