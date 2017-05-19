#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

; Inspired by Remko Trançon's post on Scheme monads: https://el-tramo.be/blog/scheme-monads/

(def macroset monad-do (<- = yield if)
  
  ((_ (>>= return guard)
      (yield exp ...))
   (return exp ...))

  ((_ (>>= return guard)
      (exp ...))
   (exp ...))

  ((_ (>>= return guard)
      (name = val)
      exps ...)
   (>>= (return val)
        (fn (name) (monad-do (>>= return guard) exps ...))))
  
  ((_ (>>= return guard)
      (name <- val)
      exps ...)
   (>>= val
        (fn (name) (monad-do (>>= return guard) exps ...))))
  
  ((_ (>>= return guard)
      (if test)
      exps ...)
   (>>= (guard test)
        (fn (_) (monad-do (>>= return guard) exps ...))))

  ((_ (>>= return guard)
      (exp0 ...)
      exps ...)
   (>>= (exp0 ...)
        (fn (_) (monad-do (>>= return guard) exps ...)))))