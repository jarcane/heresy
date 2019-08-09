#lang s-exp "../private/base.rkt"

(require racket/stxparam
         "list.rkt"
         "require-stuff.rkt"
         "theory.rkt"
         "string.rkt"
         (only-in racket/base
                  define-syntax
                  gensym
                  begin
                  let*
                  for/and
                  case-lambda
                  with-handlers
                  struct
                  exn:fail
                  exn:fail?
                  raise
                  current-continuation-marks
                  equal-hash-code)
         syntax/parse/define
         (for-syntax racket/base syntax/parse unstable/syntax))

(provide (all-defined-out))

(define-simple-macro (define-thing-literal-ids id:id ...)
  (begin (define-syntax-parameter id
           (lambda (stx)
             (raise-syntax-error #f "cannot be used outside a thing definition" stx)))
         ...))

(define-thing-literal-ids Self extends inherit super)

(def (alist-ref alist fld)
  (head (tail (assoc fld alist))))

(define-simple-macro (def-field-id id:id ths:id)
  (define-syntax id
    (make-variable-like-transformer #'(ths 'id))))

(define-simple-macro (define-syntax-parser name:id opt-or-clause ...)
  (define-syntax name (syntax-parser opt-or-clause ...)))

(define-simple-macro (build-type-list (field (type arg0 ...)) ...)
  (list
   `(field (,(partial type arg0 ...) (type arg0 ...))) ...))

(def fn empty-type-list (fields)
  (for (x in fields with Null)
    (carry (join `(,x (,any? (any?)))
                 cry))))

(def fn get-type-pred (name types)
  (head (alist-ref types name)))

(def fn get-type-name (name types)
  (head (tail (alist-ref types name))))

; (describe *thing* (*field* *value*) ...)
; Declare a new kind of Thing, with the given fields and default values.
(define-syntax-parser describe #:literals (extends inherit super)
  [(describe name:id extends super-thing:expr
             (~or (~optional (~seq inherit (inherit-id:id ...)) #:defaults ([(inherit-id 1) '()]))
                  (~optional (~seq super ([super-id1:id super-id2:id] ...))
                             #:defaults ([(super-id1 1) '()] [(super-id2 1) '()])))
             ...
             (field:id (type?:id arg0:expr ...) value:expr) ...)
   #'(def name (thing extends super-thing inherit (inherit-id ...) super ([super-id1 super-id2] ...)
                      (field (type? arg0 ...) value) ...))]
  [(describe name:id extends super-thing:expr
             (~or (~optional (~seq inherit (inherit-id:id ...)) #:defaults ([(inherit-id 1) '()]))
                  (~optional (~seq super ([super-id1:id super-id2:id] ...))
                             #:defaults ([(super-id1 1) '()] [(super-id2 1) '()])))
             ...
             (field:id value:expr) ...)
   #'(def name (thing extends super-thing inherit (inherit-id ...) super ([super-id1 super-id2] ...)
                      (field value) ...))]
  [(describe name:id (field:id (type?:id arg0:expr ...) value:expr) ...)
   #'(def name (thing (field (type? arg0 ...) value) ...))]
  [(describe name:id (field:id value:expr) ...)
   #'(def name (thing (field value) ...))])

(define-syntax-parser thing #:literals (extends inherit super)
  [(thing (field:id (type?:id arg0:expr ...) value:expr) ...)
   #'(let ([types (build-type-list (field (type? arg0 ...)) ...)])
       (make-thing `([field
                      ,(let ([field
                              (fn (ths)
                                  (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                    (def-field-id field ths) ...
                                    value))])
                         field)]
                     ...)
                   types))]
  [(thing (field:id value:expr) ...)
   #'(make-thing `([field
                    ,(let ([field
                            (fn (ths)
                              (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                (def-field-id field ths) ...
                                value))])
                       field)]
                   ...))]
  [(thing extends super-thing:expr
          (~or (~optional (~seq inherit (inherit-id:id ...)) #:defaults ([(inherit-id 1) '()]))
               (~optional (~seq super ([super-id1:id super-id2:id] ...))
                          #:defaults ([(super-id1 1) '()] [(super-id2 1) '()])))
          ...
          (field:id (type?:id arg0:expr ...) value:expr) ...)
   #'(let* ([super super-thing]
            [super-λlst (super λlst-sym)]
            [super-parents (super '__parents)]
            [super-ident (super '__ident)]
            [super-types (super '__types)]
            [types (alist-merge super-types (build-type-list (field (type? arg0 ...)) ...))])
       (make-thing (alist-merge
                    super-λlst
                    `([field
                       ,(let ([field
                               (fn (ths)
                                 (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                   (def-field-id field ths) ...
                                   (def-field-id inherit-id ths) ...
                                   (def super-id1 ((alist-ref super-λlst 'super-id2) ths)) ...
                                   value))])
                          field)]
                      ...))
                   types
                   (join super-ident super-parents)))]
  [(thing extends super-thing:expr
          (~or (~optional (~seq inherit (inherit-id:id ...)) #:defaults ([(inherit-id 1) '()]))
               (~optional (~seq super ([super-id1:id super-id2:id] ...))
                          #:defaults ([(super-id1 1) '()] [(super-id2 1) '()])))
          ...
          (field:id value:expr) ...)
   #'(let* ([super super-thing]
            [super-λlst (super λlst-sym)]
            [super-parents (super '__parents)]
            [super-ident (super '__ident)]
            [super-fields (super 'fields)]
            [super-types (super '__types)]
            [types (alist-merge super-types
                                (build-type-list (field (any?)) ...))])
       (make-thing (alist-merge
                    super-λlst
                    `([field
                       ,(let ([field
                               (fn (ths)
                                 (syntax-parameterize ([Self (make-rename-transformer #'ths)])
                                   (def-field-id field ths) ...
                                   (def-field-id inherit-id ths) ...
                                   (def super-id1 ((alist-ref super-λlst 'super-id2) ths)) ...
                                   value))])
                          field)]
                      ...))
                   types
                   (join super-ident super-parents)))])

(def λlst-sym (gensym 'λlst))
(struct exn:bad-thing-ref exn:fail ())
(struct exn:thing-type-err exn:fail ())

(def fn make-thing (λlst (types Null) [parents Null] [ident (gensym 'thing)])
  (let ()
    (def this
      (fn args*
        (let* ([alst lst]
               [hash (equal-hash-code lst)]
               [fields (heads lst)]
               [type-list (if (null? types) then (empty-type-list fields) else types)])
          (select
           [(null? args*) alst]
           [(eq? 'fields (head args*)) fields]
           [(eq? '__hash (head args*)) hash]
           [(eq? '__ident (head args*)) ident]
           [(eq? '__parents (head args*)) parents]
           [(eq? '__types (head args*)) type-list]
           [(eq? λlst-sym (head args*)) λlst]
           [(and (symbol? (head args*))
                 (assoc (head args*) alst)) (alist-ref alst (head args*))]
           [(list-of? list? (head args*))
            (let ([new-lst (for (x in (head args*) with λlst)
                             (let ([pred? (get-type-pred (head x) type-list)]
                                   [type (get-type-name (head x) type-list)])
                               (if (pred? (head (tail x)))
                                   then
                                   (carry (subst (head x) (fn (_) (head (tail x))) cry))
                                   else
                                   (raise (exn:thing-type-err
                                           (format$ "Thing encountered type error in assignment: #_ must be #_" (head x) type)
                                           (current-continuation-marks))))))])
              (make-thing new-lst types parents ident))]
           [(list? (head args*)) 
            (let recur ([λl λlst]
                        [pat (head args*)]
                        [c 1])
              (select
               [(null? pat) (make-thing λl types parents ident)]
               [(eq? (head pat) '*) (recur λl (tail pat) (+ 1 c))]
               [else
                (let* ([hd (head pat)]
                       [pair (index c type-list)]
                       [field (head pair)]
                       [type (get-type-name field type-list)]
                       [pred? (get-type-pred field type-list)])
                  (if (pred? hd)
                      then
                      (recur (subst (head (index c λl))
                                    (fn (_) hd)
                                    λl)
                        (tail pat)
                        (+ 1 c))
                      else
                      (raise (exn:thing-type-err
                              (format$ "Thing encountered type error in assignment: #_ must be #_" field type)
                              (current-continuation-marks)))))]))]
           [else (raise (exn:bad-thing-ref
                         "Thing expected a valid symbol or a pattern"
                         (current-continuation-marks)))]))))
    (def lst
      (map (fn (p)
             (list (index 1 p) ((index 2 p) this)))
           λlst))
    (if (null? types) then this else
        (do
          (for (x in lst)
            (let* ([val (head (tail x))]
                   [field (head x)]
                   [type (get-type-name field types)]
                   [pred? (get-type-pred field types)])
              (if (pred? val)
                  then val
                  else (raise (exn:thing-type-err
                               (format$ "Thing encountered type error in declaration: #_ must be #_" field type)
                               (current-continuation-marks))))))
          this))))

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

; (thing? v)
; Any -> Bool
; Returns True if value is a Thing
(def fn thing? (v)
  (and (fn? v)
       (with-handlers ((exn:bad-thing-ref? (fn (e) False)))
         (and (list? (v))
              (list? (v 'fields))
              (v '__hash)
              (fn? (v '()))))))

; (is-a? Type Thing)
; Thing Thing -> Bool
; Returns True if Thing is the same type as Type
(def fn is-a? (Type Thing)
  (and (thing? Type)
       (thing? Thing)
       (or (equal? (Type '__ident)
                   (Thing '__ident))
           (number? (inlst (Type '__ident)
                              (Thing '__parents))))))

; (thing=? thing1 thing2)
; Thing Thing -> Bool
; Returns True if both Things are the same type and their hash values are equal?
(def fn thing=? (thing1 thing2)
  (and (is-a? thing1 thing2)
       (equal? (thing1 '__hash)
               (thing2 '__hash))))

; (any? v)
; Any -> True
; Always returns True regardless of value
(def fn any? (v) True)

; (list-of? pred? xs)
; Fn(Any -> Bool) List(Any) -> Bool
; Returns True of all elements in the list match pred?
(def fn list-of? (pred? xs)
    (select
     ((null? xs) True)
     ((not (pred? (head xs))) False)
     (else (list-of? pred? (tail xs)))))

;; Placeholder values
;; These are simple values predefined for common primitive types, to provide more self-documenting default values for newly described things
;; Note that no error checking is actually performed, these are merely for documentation purposes
;; Placeholders that take an argument allow you to also specify what type is within the container, ie. (List Number)
(def Any Null)
(def String "")
(def Number 0)
(def Boolean False)
(def Symbol 'default)
(def Fn (fn (v) v))
(def Thing (thing))
(def fn List (type) (list type))

;; alist-merge
(def alist-merge
  (case-lambda
    [() '()]
    [(a) a]
    [(a b)
     (select
      [(null? b) a]
      [(null? a) b]
      [else (let* ([b.fst (head b)]
                   [b.rst (tail b)]
                   [a.hds (map head a)]

                   [b.fst.fst (head b.fst)]
                   [b.fst.rst (tail b.fst)])
              (select
               [(inlst b.fst.fst a.hds) (alist-merge (subst* b.fst.fst b.fst.rst a) b.rst)]
               [else (alist-merge (append a (list b.fst)) b.rst)]))])]
    [(a b . rst) (apply alist-merge (alist-merge a b) rst)]))

