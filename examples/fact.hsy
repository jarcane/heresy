#lang s-exp heresy

; Classical recursive
(def fn fact (n)
  (select
   ((zero? n) 1)
   (else (* n (fact (- n 1))))))

; W/ Heresy's for w/carry
(def fn fact-2 (n)
  (for (x in (range n to 1 step -1))
    (if (null? cry) 
     then
       (carry x)
     else
       (carry (* cry x)))))