#lang heresy

; Classical recursive
(def fn fact (n)
  (select
   ((zero? n) 1)
   (else (* n (fact (- n 1))))))

; W/ Heresy's for w/carry
(def fn fact-2 (n)
  (for (x in (range n to 1 step -1) with 1)
    (carry (* cry x))))

; Folding over a range
(def fn fact-3 (n)
  (foldl * 1 (range 1 to n)))

; Using the m-block
(def fn fact-4 (n)
  (if (zero? n) then 1 else 
      (m let next = n - 1
         let x = (fact-4 next)
         in x * n)))