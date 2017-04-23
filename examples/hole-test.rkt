#lang heresy

(def foo (hole 1))
(reset foo 2)
(deref foo)
(update foo + 5)
(deref foo)
(reset foo (thing (foo 1)))
((deref foo))
(reset-thing foo (foo 2))
((deref foo))