#lang racket

(require racket/generator)

(provide timer
         randomize
         rnd)

; timer
; a special internal variable that returns the current time in ms
(define-syntax timer
  (syntax-id-rules (timer) (timer (current-milliseconds))))

; (randomize [seed])
; returns a new pseudorandom number generator function
; Numbers are between 0 and 1 exclusive
; if seed is not provided, defaults to the last 32-bits of current time in ms
(define-syntax randomize
  (syntax-rules ()
    [(_ seed) 
     (generator 
      ()
      (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
        (random-seed (modulo seed (sub1 (expt 2 31))))
        (let loop ()
          (yield (random))
          (loop))))]
    [(_) (randomize timer)]))

; (rnd)
; returns a random number between 0 and 1 exclusive
(define rnd (randomize))
