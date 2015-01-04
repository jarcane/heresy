#lang racket

(require racket/stxparam)
(provide (all-defined-out)
         )

; (using (file "name" as *'input|'output*) ...)
; parameterizes the current I/O port as indicated, redirecting print & input
(define-syntax using 
  (syntax-rules (as file)
    [(_ (file name as type) body ...) 
     (cond 
       [(eq? type 'output) (with-output-to-file name
                             (lambda () body ...))]
       [(eq? type 'rewrite) (with-output-to-file name
                              #:exists 'truncate
                              (lambda () body ...))]
       [(eq? type 'input) (with-input-from-file name
                            (lambda () body ...))])]))

(define-syntax-parameter as 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "as must be used with using")))
(define-syntax-parameter file 
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "file must be used with using")))