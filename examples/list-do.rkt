#lang heresy

(def fn flatten (lst)
  (select
    ((null? lst) Null)
    ((list? lst)
     (append (flatten (head lst)) (flatten (tail lst))))
    (else (list lst))))

(def fn list-bind (lst fn)
  (flatten (map fn lst)))

(def fn list-guard (test)
  (if test then (list Null) else Null))

(def macroset list-do (= <- if yield)
  ((_ (yield exp ...)) (list exp ...))  
  ((_ (name = val) exp ...)
   (list-bind (list val) (fn (name) (list-do exp ...))))
  ((_ (name <- val) exp ...)
   (list-bind val (fn (name) (list-do exp ...))))
  ((_ (if test) exp ...)
   (list-bind (list-guard test) (fn (name) (list-do exp ...))))
  )

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