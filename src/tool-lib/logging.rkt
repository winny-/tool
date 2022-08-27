#lang racket

(require racket/logging
         racket/date
         (for-syntax syntax/parse racket/syntax-srcloc)
         ansi-color
         dirname)

(provide panic err warn info debug *log-level* logging-to-console)

(define-logger tool #:parent #f)

(define finished-logging-panic (make-channel))

#;
(define-syntax (define-logger stx)
  (syntax-parse stx
    [(_ name:id log-level:id) #'(define-logger name log-level (void))]
    [(_ name:id log-level:id after:expr)
     #`(define-syntax (name this-stx)
         (syntax-parse this-stx
           [(_ fmt:string args ...)
            #'(begin
                (log-message tool-logger name (format fmt args ...) #,(syntax-srcloc this-stx))
                after)]))]))


(define-syntax panic
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string args ...)
       #`(begin
           (log-message tool-logger 'fatal (format fmt args ...) #,(syntax-srcloc stx))
           (sync finished-logging-panic) ; Wait for log handler to finish before terminating.
           (exit 1))])))



(define-syntax err
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string args ...)
       #`(begin
           (log-message tool-logger 'error (format fmt args ...) #,(syntax-srcloc stx)))])))

(define-syntax warn
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string args ...)
       #`(begin
           (log-message tool-logger 'warning (format fmt args ...) #,(syntax-srcloc stx)))])))

(define-syntax info
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string args ...)
       #`(begin
           (log-message tool-logger 'info (format fmt args ...) #,(syntax-srcloc stx)))])))

(define-syntax debug
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string args ...)
       #`(begin
           (log-message tool-logger 'debug (format fmt args ...) #,(syntax-srcloc stx)))])))

(define colors `#hasheq((fatal . (black red))
                        (error . (red))
                        (warning . (yellow))
                        (info . (green))
                        (debug . (magenta))))

(define *log-level* (make-parameter 'info))

(define (logging-to-console proc)
  (with-intercepted-logging
    (λ (vec)
      (match-define (vector log-level msg loc sym) vec)
      (define logline
        (parameterize ([date-display-format 'iso-8601])
          (match-define (struct srcloc (source line _ _ _)) loc)
          (format "~a ~a:~a ~a ~a"
                  (date->string (current-date) #t)
                  (match source
                    [(or (? string? s) (and (? path?) (app path->string s))) (basename s)])
                  line
                  (string-upcase (~a log-level))
                  (if sym
                      (string-replace msg (format "~a: " sym) "" #:all? #f) ; Delete topic, it's not a feature we want.
                      msg))))
      (match (hash-ref colors log-level)
        [(list foreground maybe-background-xs ...)
         (parameterize ([foreground-color foreground])
           ((match maybe-background-xs
              [(list background) (λ (proc) (parameterize ([background-color background]) (proc)))]
              [(list) (λ (proc) (proc))])
            (thunk (color-displayln logline (current-error-port)))))])
      (when (symbol=? log-level 'fatal)
        (channel-put finished-logging-panic #t)))
    proc
    #:logger tool-logger
    (*log-level*)))

(define (func)
  (debug "You shouldn't see this message")
  (info "Running Racket ~a" (version))
  (err "Something went wrong but I can continue...")
  (panic "oops i died")
  ;; Not reached.
  (warn "Never complained of"))

(module+ main
  ;; (*log-level* 'debug)
  (logging-to-console func))


