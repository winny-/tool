#lang racket/base

(define-syntax-rule (reprovide thing)
  (begin
    (require thing)
    (provide (all-from-out thing))))

(reprovide "logging.rkt")
(reprovide "shell.rkt")
(reprovide "values.rkt")
(provide with-directory)

(define-syntax-rule (with-directory dir body ...)
  (parameterize ([current-directory dir])
    body ...))
