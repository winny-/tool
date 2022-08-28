#lang racket

(require racket/pretty
         racket/hash
         racket/os
         racket/logging

         basedir
         dirname
         shell/pipeline
         ansi-color

         "logging.rkt"
         "values.rkt")

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
           #:raise? [raise? #t]
           . pipeline)
  (debug
   "$ ~a"
   ;; Build a shell-like representation of the pipeline.
   (pipeline->string pipeline))
  (define execution
    (apply run-subprocess-pipeline
           pipeline
           #:in (or input-port null-redirect)
           #:out output-port
           #:err (or error-port stdout-redirect)))
  (when raise?
    (define statuses (pipeline-status/list execution))
    (match (filter (negate zero?) statuses)
      [(list) (void)]
      [(list _ ... )
       (raise-shell-error #:codes statuses #:pipeline pipeline
                          "pipeline \"~a\" failed with ~v"
                          (pipeline->string pipeline)
                          (match statuses
                            [(list code) code]
                            [many many]))]))
  execution)

(define ($/string #:raise? [raise? #t] #:error [error-port (current-output-port)] . pipeline)
  (define execution #f)
  (define s
    (string-trim
     (call-with-output-string
      (λ (out)
        (set! execution (apply $ pipeline #:raise? raise? #:output out #:error error-port))))))
  (values execution s))

(define (~ . elems)
  (apply build-path (expand-user-path "~") (map ~a elems)))

(define (// . elems)
  (apply build-path "/" (map ~a elems)))

(define (./ . elems)
  (apply build-path "." (map ~a elems)))
