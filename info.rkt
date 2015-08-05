#lang info
(define name "Heresy")
(define collection "heresy")
(define blurb
  "A BASIC-Flavored Lisp dialect")
(define scribblings '(["docs/heresy.scrbl" ()]))

(define deps '(("base" #:version "6.2.900.6")))
(define build-deps '("racket-doc"
                     "rackunit-lib" "sandbox-lib" "scribble-lib"))
