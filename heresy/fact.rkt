#lang racket

(require "main.rkt")

(def fn fact (n)
  (select
   ((= n 1) 1)
   (else (* n (fact (- n 1))))))