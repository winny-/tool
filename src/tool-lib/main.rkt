#lang racket/base

(require racket/list
         racket/match)

(define-syntax-rule (reprovide thing)
  (begin
    (require thing)
    (provide (all-from-out thing))))

(reprovide "logging.rkt")
(reprovide "shell.rkt")
(reprovide "values.rkt")
(provide with-directory tool-init!)

(define-syntax-rule (with-directory dir body ...)
  (parameterize ([current-directory dir])
    body ...))

(define (tool-init! #:logging [logging #t])
  (tool-init-logging! logging))

