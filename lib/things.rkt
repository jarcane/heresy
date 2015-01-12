#lang s-exp "../private/base.rkt"

(require racket/stxparam
         "list.rkt"
         "require-stuff.rkt"
         (only-in racket/base define-syntax gensym begin let*)
         syntax/parse/define
         (for-syntax racket/base syntax/parse unstable/syntax))

(provide (all-defined-out))

(define-simple-macro (define-thing-literal-ids id:id ...)
  (begin (define-syntax-parameter id
           (lambda (stx)
             (raise-syntax-error #f "cannot be used outside a thing definition" stx)))
         ...))

(define-thing-literal-ids Self extends inherit)

(def (alist-ref alist fld)
  (head (tail (assoc fld alist))))

(define-simple-macro (def-field-id id:id ths:id)
  (define-syntax id
    (make-variable-like-transformer #'(ths 'id))))

(define-simple-macro (define-syntax-parser name:id opt-or-clause ...)
  (define-syntax name (syntax-parser opt-or-clause ...)))

; (describe *thing* (*field* *value*) ...)
; Declare a new kind of Thing, with the given fields and default values.
(define-syntax-parser describe #:literals (extends inherit)
  [(describe name:id extends super-thing:expr inherit (inherit-id:id ...) (field:id value:expr) ...)
   #'(def name (thing extends super-thing inherit (inherit-id ...) (field value) ...))]
  [(describe name:id extends super-thing:expr (field:id value:expr) ...)
   #'(def name (thing extends super-thing (field value) ...))]
  [(describe name:id (field:id value:expr) ...)
   #'(def name (thing (field value) ...))])

(define-syntax-parser thing #:literals (extends inherit)
  [(thing (field:id value:expr) ...)
   #'(make-thing `([field
                    ,(let ([field
                            (fn (ths)
                              (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                (def-field-id field ths) ...
                                value))])
                       field)]
                   ...))]
  [(thing extends super-thing:expr inherit (inherit-id:id ...) (field:id value:expr) ...)
   #'(let ([super super-thing])
       (make-thing (append (filter (fn (x) (not (inlst (head x) '(field ...))))
                                   (super λlst-sym))
                           `([field
                              ,(let ([field
                                      (fn (ths)
                                        (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                          (def-field-id field ths) ...
                                          (def-field-id inherit-id ths) ...
                                          value))])
                                 field)]
                             ...))))]
  [(thing extends super-thing:expr (field:id value:expr) ...)
   #'(thing extends super-thing inherit () (field value) ...)])

(def λlst-sym (gensym 'λlst))

(def fn make-thing (λlst)
  (let ()
    (def this
      (fn args*
        (let ([alst lst]
              [fields (heads lst)])
          (select
           [(null? args*) alst]
           [(eq? 'fields (head args*)) fields]
           [(eq? λlst-sym (head args*)) λlst]
           [(and (symbol? (head args*))
                 (assoc (head args*) alst)) (alist-ref alst (head args*))]
           [(list? (head args*)) 
            (let recur ([λl λlst]
                        [pat (head args*)]
                        [c 1])
              (select
               [(null? pat) (make-thing λl)]
               [(eq? (head pat) '*) (recur λl (tail pat) (+ 1 c))]
               [else
                (let ([hd (head pat)])
                  (recur (subst (head (index c λl))
                                (list (fn (_) hd))
                                λl)
                    (tail pat)
                    (+ 1 c)))]))]
           [else (error "Thing expected a valid symbol or a pattern")]))))
    (def lst
      (map (fn (p)
             (list (index* p 1) ((index* p 2) this)))
           λlst))
    this))

(def (send thing method . args)
  (apply (thing method) args))

(define-simple-macro (send* obj-expr:expr (method:id arg ...) ...+)
  (let ([obj obj-expr])
    (send obj 'method arg ...)
    ...))

(define-simple-macro (send+ obj-expr:expr msg:expr ...)
  (let* ([obj obj-expr]
         [obj (send* obj msg)] ...)
    obj))

