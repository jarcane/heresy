#lang heresy

(def foo (hole 1))
(hole? foo)
(reset foo 2)
(deref foo)
(update foo + 5)
(deref foo)
(reset foo (thing (foo 1)))
((deref foo))
(reset-thing foo (foo 2))
((deref foo))

(hole-do
 (a <- (hole 1))
 (b <- (hole 2))
 (z = (+ a b))
 (print z)
 (yield z))