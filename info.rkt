#lang info
(define name "Heresy")
(define collection "heresy")
(define version "0.2.0")
(define blurb
  "A BASIC-Flavored Lisp dialect")
(define scribblings '(["docs/heresy.scrbl" (multi-page) (language)]))

(define deps '("base" "unstable-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib" "sandbox-lib" "scribble-lib"))
