#lang s-exp heresy

(require rackunit
         "../lib/things.rkt")
(import rkt racket/base)

(test-case "cthulhu"
  (describe cthulhu
            (size 'massive)
            (type 'squamous)
            (status 'sleeping))
  
  (def great-baalthogua (cthulhu '(* * awake)))
  
  (check-equal? (cthulhu)
                '((size massive) (type squamous) (status sleeping)))
  (check-equal? (great-baalthogua)
                '((size massive) (type squamous) (status awake)))
  (check-equal? (cthulhu 'size) 'massive)
  (check-equal? ((cthulhu '(* * awake)) 'status) 'awake)
  (def dreamer cthulhu)
  (check-equal? (dreamer 'type) 'squamous)
  (def star-spawn (cthulhu '(medium * awake)))
  (check-equal? (star-spawn) '((size medium) (type squamous) (status awake)))
  (check-equal? (star-spawn 'size) 'medium)
  )

(test-case "using methods"
  (def (make-fish sz)
    (let ()
      (describe my-fish
                [size sz]
                [get-size (fn () size)]
                [grow (fn (amt)
                        (Self `(,(+ amt size))))]
                [eat (fn (other-fish)
                       (grow (other-fish 'get-size)))])
      my-fish))
  (def charlie (make-fish 10))
  (check-equal? (charlie 'size) 10)
  (def charlie2 (send charlie 'grow 6))
  (check-equal? (charlie2 'size) 16)
  (check-equal? (send charlie2 'get-size) 16)
  (check-equal? (send charlie 'get-size) 10)
  )

(test-case "make sure the field exprs aren't re-evaluated"
  (def x (rkt:box 1))
  (def (get-x) (rkt:unbox x))
  (def (inc-x!) (rkt:set-box! x (inc (get-x))))
  (describe foo [a (inc-x!)])
  (check-equal? (get-x) 2)
  (foo 'a)
  (check-equal? (get-x) 2)
  (foo 'a)
  (check-equal? (get-x) 2)
  )
