#lang racket

(require racket/unsafe/ops
         racket/generator)

(provide timer
         randomize
         rnd)

(define fxxor unsafe-fxxor)
(define fxrshift unsafe-fxrshift)
(define fxlshift unsafe-fxlshift)
(define fx* unsafe-fx*)

; timer
; a special internal variable that returns the current time in ms
(define-syntax timer
  (syntax-id-rules (timer) (timer (current-milliseconds))))

; (randomize [seed])
; returns a new pseudorandom number generator function
; Numbers are between 0 and 1 exclusive
; method is the xorshift* algorithm
; if seed is not provided, defaults to the current time in ms
(define-syntax randomize
  (syntax-rules ()
    [(_ seed) 
     (generator 
      ()
      (let loop ([x (equal-hash-code seed)]
                 [f (λ (x dir y) 
                      (fxxor x (if (eq? dir 'l) 
                                   (fxlshift x y)
                                   (fxrshift x y))))])
        (begin
          (let ([new-x (f (f (f x 'r 12) 'l 25) 'r 27)])
            (yield (/ (modulo (unsafe-fxabs (fx* 2685821657736338717 new-x)) 
                              (expt 2 62))
                      (expt 2.0 62)))
            (loop new-x f)))))]
    [(_) (randomize timer)]))

; (rnd)
; returns a random number between 0 and 1 exclusive
(define rnd (randomize))
