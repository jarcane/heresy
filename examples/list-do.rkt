#lang heresy

(list-do
 (x <- (range 1 to 5))
 (y <- (range 1 to 5))
 (z = (* x y))
 (if (even? z))
 (yield z))

(list-do
 (rank <- (append (range 2 to 10) '(J Q K A)))
 (suit <- '(♠ ♣ ♥ ♦))
 (if (equal? suit '♦))
 (card = (format$ "#_#_" rank suit))
 (yield card))

(list-do
 (x <- (range 1 to 5))
 (y <- (range 1 to 5))
 (z = (+ x y))
 (yield (* z z)))