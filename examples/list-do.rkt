#lang heresy

(def fn flatten (lst)
  (select
    ((null? lst) Null)
    ((list? lst)
     (append (flatten (head lst)) (flatten (tail lst))))
    (else (list lst))))

(def fn list-bind (lst fn)
  (flatten (map fn lst)))

(def macroset list-do (= <- if yield)
  ((_ (exp ...)) (exp ...))  
  ((_ (name = val) exp ...)
   (list-bind (list val) (fn (name) (list-do exp ...))))
  ((_ (name <- val) exp ...)
   (list-bind val (fn (name) (list-do exp ...))))
  ((_ (if pred? name) exp ...)
   (list-bind (filter pred? name) (fn (name) (list-do exp ...)))))

(list-do
 (x <- (range 1 to 5))
 (y <- (range 1 to 5))
 (z = (* x y))
 (list z))