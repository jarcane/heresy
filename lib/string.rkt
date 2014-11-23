#lang s-exp "../private/base.rkt"

(require "list.rkt")

; (len$ *str*)
; Returns the length of the given str
(def fn len$ (str)
  (len (list$ str)))

; (list& *lst*)
; Takes a list of strings and returns a single string concatenated together
(def fn list& (lst)
  (apply & lst))

; (mid$ *str* *idx* *len*)
; Returns a slice of str, len long, starting at idx
(def fn mid$ (str idx length)
  (select
   ((> idx (len$ str)) (error 'mid$ "out of index"))
   ((> (+ idx length) (len$ str)) (error 'mid$ "slice too long"))
   (else (list& 
          (reverse 
           (for (x in (range idx to (+ idx length -1)))
             (carry (join (index x (list$ str)) cry))))))))