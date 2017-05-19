#lang s-exp "../private/base.rkt"

(provide (all-defined-out))

; monad-do
; The base implementation for Heresy do notation DSL
; Inspired by Remko Trançon's post on Scheme monads: https://el-tramo.be/blog/scheme-monads/
; This constructor form can be used directly, by providing the bind (>>=), return, and guard
; functions for a data type, or more commonly, wrapped in a macro that provides these
; Comments below describe individual forms/operators within the DSL
(def macroset monad-do (<- = yield if)

  ; (yield exp ...)
  ; When used as the final line of a do form, returns the result of the expression(s) wrapped
  ; in the monad's constructor
  ((_ (>>= return guard)
      (yield exp ...))
   (return exp ...))

  ; If instead the final line of the do form is a bare expression, its result will be returned
  ((_ (>>= return guard)
      (exp ...))
   (exp ...))

  ; (name = val)
  ; Wraps the expression val in the monad type, and binds it to name
  ((_ (>>= return guard)
      (name = val)
      exps ...)
   (>>= (return val)
        (fn (name) (monad-do (>>= return guard) exps ...))))

  ; (name <- val)
  ; Binds the given monadic value to name. Will fail if not the correct type
  ((_ (>>= return guard)
      (name <- val)
      exps ...)
   (>>= val
        (fn (name) (monad-do (>>= return guard) exps ...))))

  ; (if test)
  ; The guard pattern. If test is true, will bind to an instance of the type, else to Null
  ((_ (>>= return guard)
      (if test)
      exps ...)
   (>>= (guard test)
        (fn (_) (monad-do (>>= return guard) exps ...))))

  ; Bare expressions within the do-form are evaluated but their results ignored
  ((_ (>>= return guard)
      (exp0 ...)
      exps ...)
   (>>= (return (exp0 ...))
        (fn (_) (monad-do (>>= return guard) exps ...)))))