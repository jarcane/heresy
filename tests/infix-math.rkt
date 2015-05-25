#lang heresy

;; based on maya.clj:
;; https://gist.github.com/divs1210/b4fcbd48d7697dfd8850
;; http://pizzaforthought.blogspot.in/2015/01/maya-dsl-for-math-and-numerical-work.html

(require "../lib/infix-math.rkt"
         "../lib/require-stuff.rkt"
         rackunit
         (only-in racket/base sqrt))

(def macro ~> (a b) (b a))

(test-case "quadratic1"
  (def fn quadratic (a b c)
    (m let d = 4 * a * c
       let D = b * b - d ~> sqrt
       let t = 2 * a let -b = (- b)
       let x1 = -b + D / t
       let x2 = -b - D / t in
       (list x1 x2)))
  (check-equal? (quadratic 1 0 0) '(0 0))
  (check-equal? (quadratic 1 0 -1) '(1 -1))
  (check-equal? (quadratic 1 0 -4) '(2 -2))
  (check-equal? (quadratic 1 0 1) '(+i -i))
  (check-equal? (quadratic 1 0 4) '(+2i -2i))
  (check-equal? (quadratic 1 -2 0) '(2 0))
  (check-equal? (quadratic 1 2 0) '(0 -2))
  (check-equal? (quadratic 1 -2 1) '(1 1))
  (check-equal? (quadratic 1/2 -2 2) '(2 2))
  (check-equal? (quadratic 1/2 -2 0) '(4 0)))

(test-case "quadratic2"
  (def fn quadratic (a b c)
    (m with (- b) as -b
       with b ^ 2 - (m 4 * a * c) ~> sqrt as D
       let x1 = -b + D / (m 2 * a)
       let x2 = -b - D / (m 2 * a) in
       (list x1 x2)))
  (check-equal? (quadratic 1 0 0) '(0 0))
  (check-equal? (quadratic 1 0 -1) '(1 -1))
  (check-equal? (quadratic 1 0 -4) '(2 -2))
  (check-equal? (quadratic 1 0 1) '(+i -i))
  (check-equal? (quadratic 1 0 4) '(+2i -2i))
  (check-equal? (quadratic 1 -2 0) '(2 0))
  (check-equal? (quadratic 1 2 0) '(0 -2))
  (check-equal? (quadratic 1 -2 1) '(1 1))
  (check-equal? (quadratic 1/2 -2 2) '(2 2))
  (check-equal? (quadratic 1/2 -2 0) '(4 0)))

