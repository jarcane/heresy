#lang heresy

(def fn fizzbuzz (n)
  (for (x in (range 1 to n))
    (select
     ((zero? x) x)
     ((zero? (+ (mod x 5)
                (mod x 3))) (print "FizzBuzz"))
     ((zero? (mod x 5)) (print "Buzz"))
     ((zero? (mod x 3)) (print "Fizz"))
     (else (print x)))))
