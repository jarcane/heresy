#lang heresy

(def foo (hole 1))

(deref foo)

(update foo + 1 2 3)

(deref foo)

;(update foo / 0)