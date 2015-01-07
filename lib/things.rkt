#lang s-exp "../private/base.rkt"

(require racket/stxparam
         "list.rkt"
         "require-stuff.rkt"
         (only-in racket/base procedure? define-syntax-rule define-syntax)
         (for-syntax racket/base unstable/syntax))

(provide (all-defined-out))

(define-syntax-parameter Self
  (lambda (stx)
    (raise-syntax-error #f "cannot be used outside a thing definition" stx)))

(def (alist-ref alist fld)
  (head (tail (assoc fld alist))))

(define-syntax-rule (def-field-id id ths)
  (define-syntax id
    (make-variable-like-transformer #'(ths 'id))))

; (describe *thing* (*field* *value*) ...)
; Declare a new kind of Thing, with the given fields and default values.
(def macro describe (name (field value) ...)
  (def name
    (thing `([field ,(let ([field
                            (fn (ths)
                              (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                (def-field-id field ths) ...
                                value))])
                       field)]
             ...))))

(def fn thing (λlst)
  (let ()
    (def this
      (fn args*
        (let ([alst lst]
              [fields (heads lst)])
          (select
           [(null? args*) alst]
           [(eq? 'fields (head args*)) fields]
           [(and (symbol? (head args*))
                 (assoc (head args*) alst)) (alist-ref alst (head args*))]
           [(list? (head args*)) 
            (let recur ([λl λlst]
                        [pat (head args*)]
                        [c 1])
              (select
               [(null? pat) (thing λl)]
               [(eq? (head pat) '*) (recur λl (tail pat) (+ 1 c))]
               [(and (list? (head pat)) (= (len (head pat)) 2) (eq? (head (head pat)) '#:m)
                     (procedure? (head (tail (head pat)))))
                (recur (subst (head (index c λl))
                              (list (head (tail (head pat))))
                              λl)
                  (tail pat)
                  (+ 1 c))]
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
