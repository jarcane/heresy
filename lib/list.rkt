#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

; (map *fun* *lst*)
; Takes a single-argument function and list,
; returning a new list with the function applied to each entry.
(def fn map (fun lst)
  (select
   ((null? lst) lst)
   (else (join (fun (head lst))
               (map fun (tail lst))))))

; (filter *pred* *lst*)
; Returns a new list, containing only those items which are True according to pred
(def fn filter (pred lst)
  (select
   ((null? lst) lst)
   ((pred (head lst))
    (join (head lst)
          (filter pred (tail lst))))
    (else (filter pred (tail lst)))))

; (len *lst*)
; Returns the number of items in the list
(def fn len (lst)
  (select
   ((null? lst) 0)
   (else (+ 1 (len (tail lst))))))

; (foldr *fun* *base* *lst*)
; Folds a list from the right, combining pairs with fun, and returns the result
(def fn foldr (fun base lst)
  (select
   ((null? lst) base)
   (else (fun (head lst) (foldr fun base (tail lst))))))

; (foldl *fun* *base* *lst*)
; Folds a list from the left
(def fn foldl (fun base lst)
  (select
   ((null? lst) base)
   (else (foldl fun (fun (head lst) base) (tail lst)))))

; (reverse *lst*)
; Returns list with items in reverse order
(def fn reverse (lst)
  (foldl join Null lst))