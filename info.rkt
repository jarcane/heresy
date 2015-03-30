#lang info
(define name "Heresy")
(define collection "heresy")
(define blurb
  "A BASIC-Flavored Lisp dialect")
(define scribblings '(["docs/heresy.scrbl" ()]))

(define deps '("base"))
(define build-deps '("rackunit-lib" "sandbox-lib" "scribble-lib"))
