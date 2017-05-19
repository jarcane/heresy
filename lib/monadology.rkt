#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

(def macroset monad-do (<- := yield if)
  
  ((_ (>>= return guard)
      (yield exp ...))
   (return exp ...))

  ((_ (>>= return guard)
      (name := exp)
      exps ...)
   (>>= (return exp)
        (fn (name) (monad-do (>>= return guard) exps ...))))
  
  ((_ (>>= return guard)
      (name <- exp)
      exps ...)
   (>>= exp
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