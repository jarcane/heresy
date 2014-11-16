#lang s-exp heresy

(def fn fact (n)
  (select
   ((zero? n) 1)
   (else (* n (fact (- n 1))))))
