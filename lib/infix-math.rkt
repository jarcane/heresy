#lang s-exp "../private/base.rkt"

;; based on maya.clj:
;; https://gist.github.com/divs1210/b4fcbd48d7697dfd8850
;; http://pizzaforthought.blogspot.in/2015/01/maya-dsl-for-math-and-numerical-work.html

(provide m)

(require "require-stuff.rkt"
         racket/stxparam
         syntax/parse/define
         (only-in racket/base define-syntax begin-for-syntax)
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-simple-macro (define-syntax-parser macro:id opt-or-clause ...)
  (define-syntax macro (syntax-parser opt-or-clause ...)))

(begin-for-syntax
  (define-syntax-class !lit #:literals (let = in with as)
    [pattern (~not (~or let = in with as))]))

(define-syntax-parser m #:literals (let = in with as)
  [(m a) #'a]
  [(m let ~! a-id:id = a:!lit ...
      (~or (~seq in b ...)
           (~and (~seq (~or let with) _ ...) (~seq b ...))))
   #'(let ([a-id (m a ...)])
       (m b ...))]
  [(m with ~! a:!lit ... as a-id:id (~optional in) b ...)
   #'(let ([a-id (m a ...)])
       (m b ...))]
  [(m a op b . rst)
   #'(m (op a b) . rst)])

