#lang s-exp "../private/base.rkt"

(require "math.rkt")
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

; (index *nth* *lst*)
; Returns the nth entry in lst. 1-indexed.
(def fn index (nth lst)
  (select
   ((> nth (len lst)) (error 'index "out of index"))
   ((one? nth) (head lst))
   (else (index (- nth 1) (tail lst)))))

; (index* *lst* . *dims*)
; Walks through nested lists according to dimensions and returns the indexed result
(def fn index* (lst . dims)
  (for (x in dims)
    (if (null? cry) 
        then
        (carry (index x lst))
        else
        (carry (index x cry)))))

; (inlst *item* *lst*)
; Searches lst for item, returns index of item if found, False if not
(def fn inlst (item lst (idx 1))
  (select
   ((null? lst) False)
   ((eq? (head lst) item) idx)
   (else (inlst item (tail lst) (+ 1 idx)))))

; (left *lst* *n*)
; Returns the first n elements of the list.
(def fn left (lst n)
  (select
   ((zero? n) Null)
   (else (join (head lst) (left (tail lst) (- n 1))))))

; (right *lst* *n*)
; Returns last n entries from the list
(def fn right (lst n)
  (reverse (left (reverse lst) n)))

; (mid *lst* *pos* *n*)
; Returns n entries from lst starting at pos
(def fn mid (lst pos n)
  (select
   ((one? pos) (left lst n))
   (else (mid (tail lst) (- pos 1) n))))

; (slice *lst* *first* *last*)
; Returns a slice of the list from first and last positions, inclusive.
(def fn slice (lst (first 0) (last (len lst)))
  (mid lst first (- last first -1)))

; (append1 *lst* *lst2*)
; Returns a list with the contents of lst2 appended to the end of lst1
(def fn append1 (lst lst2)
  (select
   ((null? lst) lst2)
   (else (join (head lst) (append (tail lst) lst2)))))

; (append *lst1* ...)
; Returns a list with the given lists appended one after the other
(def fn append *args
  (foldr append1 '() *args))

; (assoc *target* *lst*)
; Searches the heads of a list of lists/pairs, and returns the first matching list or #f
(def fn assoc (tgt lst)
  (select
   ((null? lst) False)
   ((eq? tgt (head (head lst))) (head lst))
   (else (assoc tgt (tail lst)))))

; (subst *tgt* *new* *lst*)
; returns a new list of lists with assoc of tgt's tail replaced with new
(def fn subst (tgt new lst)
  (select 
   ((null? lst) False)
   ((eq? tgt (head (head lst))) (join (join tgt new) (tail lst)))
   (else (join (head lst) (subst tgt new (tail lst))))))

; (sort *fun* *lst*)
; Sorts list according to comparator fun
(def fn sort (fun lst)
  (select 
   ((null? lst) lst)
   (else (append (sort fun (filter (fn (x) (not (fun (head lst) x))) (tail lst)))
                 (list (head lst))
                 (sort fun (filter (fn (x) (fun (head lst) x)) (tail lst)))))))

; (zip *lst1* *lst2*)
; Combines two lists into a single list of lists. Excess length is lost.
(def fn zip (lst1 lst2)
  (select
   ((or (null? lst1)
        (null? lst2)) Null)
   (else (join (list (head lst1)
                     (head lst2))
               (zip (tail lst1)
                    (tail lst2))))))

; (zipwith *fun* *lst1* *lst*)
; Returns a new list containing the result of applying fun to matching entries in lsts
(def fn zipwith (fun lst1 lst2)
  (select
   ((or (null? lst1)
        (null? lst2)) Null)
   (else (join (fun (head lst1)
                    (head lst2))
               (zipwith fun 
                        (tail lst1)
                        (tail lst2))))))