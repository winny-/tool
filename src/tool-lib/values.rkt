#lang racket

(provide [except-out (all-defined-out)
                     define-values-getter])

(require (for-syntax syntax/parse racket))

(define-syntax (define-values-getter stx)
  (syntax-parse stx
    [(_ name:id)
     (with-syntax ([binding (datum->syntax stx (string->symbol (format "values-~a" (syntax-e #'name))))])
       #'(define-syntax (binding this-stx)
           (syntax-parse this-stx
             [(_ val:expr)
              #'(name (call-with-values (λ () val) list))])))]))

(define-values-getter first)
(define-values-getter second)
(define-values-getter third)
(define-values-getter fourth)
(define-values-getter fifth)
(define-values-getter sixth)
(define-values-getter seventh)
(define-values-getter eigth)
(define-values-getter ninth)

(module+ main
  (define c 0)
  (values-second (begin (set! c (add1 c)) (values 0 c))))
