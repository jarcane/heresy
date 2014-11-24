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