#lang racket

(require tool)

(define (main)
  (for ([i (in-range 1 (add1 30))])
    (define-values (divides3 divides5)
      (values (zero? (modulo i 3)) (zero? (modulo i 5))))
    (cond
      [(and divides3 divides5)
       (debug "FizzBuzz (~a)" i)]
      [divides3
       (info "Fizz")]
      [divides5
       (warn "Buzz")]
      [else
       (err "~a" i)]))
  (panic "Ran out of numbers to fizzbuzz, giving up."))

(parameterize ([*log-level* 'debug])
  (call-with-logging-to-console main))
