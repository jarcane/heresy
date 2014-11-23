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