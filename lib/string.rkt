#lang s-exp "../private/base.rkt"

(require "list.rkt")

; (empty? str)
; Returns true if string is empty)
(def fn empty? (str)
  (=$ str ""))

; (len$ *str*)
; Returns the length of the given str
(def fn len$ (str)
  (len (list$ str)))

; (list& *lst*)
; Takes a list of strings and returns a single string concatenated together
(def fn list& (lst)
  (apply & lst))

; (head$ *str*)
; Returns the first character of the string
(def fn head$ (str)
  (head (list$ str)))

; (tail$ *str*)
; Returns the remainder of the string
(def fn tail$ (str)
  (let ([tl (list$ str)])
    (if (null? tl) then
        ""
        else (list& (tail tl)))))

; (left$ *str* *n* )
; Returns a slice of str, length long, starting at idx
(def fn left$ (str n)
  (list& (left (list$ str) n)))

; (right$ *str* *n*)
; Returns a slice of str, length long, starting at idx
(def fn right$ (str n)
  (list& (right (list$ str) n)))

; (mid$ *str* *idx* *length*)
; Returns a slice of str, length long, starting at idx
(def fn mid$ (str idx length)
  (list& (mid (list$ str) idx length)))

; (slice$ *str* *start* *finish*)
; Returns a slice of the string from start to finish, inclusive
(def fn slice$ (str (start 0) (finish (len$ str)))
  (list& (slice (list$ str) start finish)))

; (instr *str* *search*)
; Returns the index of the first instance given search string within str, or #f
(def fn instr (str search (idx 1))
  (select 
   ((empty? str) False)
   ((> (len$ search) (len$ str)) False)
   ((=$ search (left$ str (len$ search))) idx)
   (else (instr (tail$ str) search (+ 1 idx)))))