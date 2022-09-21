#lang racket/base

(require racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/string

         shell/pipeline

         "logging.rkt")

(provide $ $/string ~ // ./ (struct-out exn:shell) raise-shell-error pipeline->string)

(struct exn:shell exn:fail:user (pipeline codes))
(define (raise-shell-error #:codes [codes empty] #:pipeline pipeline msg . args)
  (raise (exn:shell
          (apply format msg args)
          (current-continuation-marks)
          pipeline
          codes)))

(define (pipeline->string pipeline)
  (string-join
   (for/list ([command pipeline])
     (string-join
      (for/list ([arg command])
        (define s (~a arg))
        (if (string-contains? s " ")
            (format "'~a'" s)
            s))
      " "))
   " | "))

(define ($ #:input [input-port #f]
           #:output [output-port (current-output-port)]
           #:error [error-port (current-error-port)]
           . pipeline)
  (debug
   "$ ~a"
   ;; Build a shell-like representation of the pipeline.
   (pipeline->string pipeline))
  (define execution
    (apply run-subprocess-pipeline
           pipeline
           #:in (or input-port null-redirect)
           #:out (or output-port null-redirect)
           #:err (or error-port stdout-redirect)
           #:strictness 'strict))
  ;; Raise an exception if there was a command failure.
  (define statuses (pipeline-status/list execution))
  (match (filter (negate zero?) statuses)
    [(list) (void)]
    [(list _ ... )
     (raise-shell-error #:codes statuses #:pipeline pipeline
                        "pipeline \"~a\" failed with ~v"
                        (pipeline->string pipeline)
                        (match statuses
                          [(list code) code]
                          [many many]))])
  execution)

(define ($/string #:error [error-port (current-output-port)] . pipeline)
  (string-trim
   (call-with-output-string
    (λ (out)
      (apply $ pipeline #:output out #:error error-port)))))

(define (~ . elems)
  (apply build-path (expand-user-path "~") (map ~a elems)))

(define (// . elems)
  (apply build-path "/" (map ~a elems)))

(define (./ . elems)
  (apply build-path "." (map ~a elems)))
