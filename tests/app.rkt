#lang s-exp heresy

(require rackunit)

(test-case "#%app for procedure applications"
  (def fn f (x) (list x 5))
  (check-equal? (f 8) '(8 5)))

(test-case "#%app for index*"
  (def dave '(1 (2 3 (4 5)) 6))
  (check-equal? (dave 2 3 1) 4)
  (def lst2 '(1 1 2 4 5 8 13 21 34 55 89 144))
  (check-equal? (map lst2 '(7 8 9)) '(13 21 34)))

