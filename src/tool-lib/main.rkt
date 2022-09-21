#lang racket/base

(require racket/list
         racket/match
         racket/path
         racket/contract
         syntax/parse
         (for-syntax syntax/parse racket/base))

(define-syntax-rule (reprovide thing)
  (begin
    (require thing)
    (provide (all-from-out thing))))

(reprovide "logging.rkt")
(reprovide "shell.rkt")
(reprovide "values.rkt")
(provide with-directory tool-init! ->path)

(define-syntax-rule (with-directory dir body ...)
  (parameterize ([current-directory dir])
    body ...))

(define/contract (->path thing . sub)
  (->* [(or/c path-string? path? symbol?)] #:rest (or/c path-string? path? symbol?) path?)
  (define base
    (expand-user-path
     (match thing
       [(? string? s) s]
       [(? symbol? sym) (string->symbol sym)])))
  (apply build-path
         base
         (for/list ([s sub])
           (match s
             [(? string? s) (string->path s)]
             [(? symbol? sym) (string->path (symbol->string s))]))))

(begin-for-syntax
  (define-syntax (first-attribute stx)
    (syntax-rules ()
      [(_ a0)
       (attribute a0)]
      [(_ a0 a* ...)
       (or (first-attribute a0) (first-attribute a* ...))]))
  (define-syntax-class log-level
    (pattern (~or debug info warning error fatal)
             #:with ->symbol
              (cond
                [(attribute debug) => (lambda (x) x)]
                [(attribute info) => (lambda (x) x)]
                [(attribute warning) => (lambda (x) x)]
                [(attribute error) => (lambda (x) x)]
                [(attribute fatal) => (lambda (x) x)]))))

(define-syntax (tool-init! stx)
  (syntax-parse stx
    [(_)
     #'(tool-init! #:logging #t)]
    [(_ #:logging #t)
     #'(tool-init! #:logging 'warning)]
    [(_ #:logging ll:expr)
     #'(begin (tool-init-logging!)
              (*log-level* ll))]))


