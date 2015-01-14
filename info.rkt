#lang info
(define name "Heresy")
(define collection "heresy")
(define blurb
  "A BASIC-Flavored Lisp dialect")
(define deps '("base"))
(define build-deps '("scribble-lib" "sandbox-lib" "rackunit-lib"))
(define scribblings '(["docs/heresy.scrbl" ()]))
