#lang heresy

(import rkt racket/base)

(describe List)
(describe Nil extends List)
(describe Cons extends List
          (car Nil)
          (cdr Nil))

(def fn cons (a b)
  (Cons (list a b)))

(def fn car (l)
  (l 'car))

(def fn cdr (l)
  (l 'cdr))

(def fn cons? (v)
  (is-a? Cons v))

(def fn nil? (l)
  (is-a? Nil l))

(def fn print-cons (l)
  (select
   ((nil? l) "Nil")
   ((cons? (car l)) (format$ "Cons(#_, #_)" (print-cons (car l)) (print-cons (cdr l))))
   (else (format$ "Cons(#_, #_)" (car l) (print-cons (cdr l))))))

(def fn cons->list (l)
  (select
   ((nil? l) Null)
   ((cons? (car l)) (join (cons->list (car l))
                        (cons->list (cdr l))))
   (else (join (car l)
               (cons->list (cdr l))))))

(def l (cons 1 (cons (cons 2 (cons 4 Nil)) (cons 3 Nil))))

(print-cons l)
(cons->list l)
(cons? l)