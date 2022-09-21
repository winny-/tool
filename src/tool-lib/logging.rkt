#lang racket/base

(require (for-syntax racket/syntax-srcloc
                     syntax/parse
                     racket/base)
         racket/bool
         racket/contract/base
         racket/contract/region
         racket/date
         racket/format
         racket/function
         racket/list
         racket/logging
         racket/match
         racket/string

         ansi-color
         dirname

         "syslog.rkt")

(module+ test
  (require rackunit))

(provide panic err warn info debug *log-level* log-level<=? log-to-stderr log-to-syslog tool-init-logging!)

(define-logger tool #:parent #f)

(define finished-logging-panic (make-channel))

(define-syntax (define-logger stx)
  (syntax-parse stx
    [(_ name:id log-level:id)
     #'(define-logger name log-level (void))]
    [(_ name:id log-level:id after:expr)
     #'(...
        (define-syntax (name this-stx)
          (syntax-parse this-stx
            [(_ fmt:string args:expr ...)
             #`(begin
                 (log-message tool-logger
                              'log-level
                              (format fmt args ...)
                              #,(syntax-srcloc this-stx))
                 after)])))]))

(define-logger panic fatal
  (begin
    (sync finished-logging-panic) ; Wait for log handler to finish before terminating.
    (exit 1)))
(define-logger err error)
(define-logger warn warning)
(define-logger info info)
(define-logger debug debug)

(define levels '(fatal error warning info debug))
(define (tool-log-level? e) (and (member e levels) #t))
(define colors `#hasheq((fatal . (black red))
                        (error . (red))
                        (warning . (yellow))
                        (info . (green))
                        (debug . (magenta))))
(define short-names #hasheq((fatal   . FTL)
                            (error   . ERR)
                            (warning . WRN)
                            (info    . INF)
                            (debug   . DBG)))

(define-syntax (define-global stx)
  (syntax-parse stx
    [(_ name:id init-e:expr)
     #'(define-global name init-e any/c)]
    [(_ name:id init-e:expr contract:expr)
     #'(begin
         (define/contract binding (box/c contract) (box init-e))
         (define (name . args)
           (match args
             [(list) (unbox binding)]
             [(list v)
              (set-box! binding v)])))]))
(define-global *log-show-filename* #t)
(define-global *log-show-date* #t)
(define-global *log-show-topic* #f)
(define-global *log-level* 'info tool-log-level?)

(define log-vector? (vector/c log-level/c string? any/c (or/c #f symbol?)))

(define/contract (make-log-vector #:log-level log-level
                         #:message message
                         #:topic [topic 'tool]
                         #:data [data #f])
  (->* (#:log-level log-level/c #:message string?)
       (#:topic (or/c #f symbol?) #:data any/c)
       log-vector?)
  (vector->immutable-vector (vector log-level message data topic)))

(define (my-error-display-handler msg ex)
  (define vec (make-log-vector #:log-level 'error
                               #:message msg
                               #:topic #f
                               #:data (and (exn:srclocs? ex)
                                           (car ((exn:srclocs-accessor ex) ex)))))
  (handle-log-message vec))

(define (log-level<=? a b)
  (<= (index-of levels a) (index-of levels b)))

(module+ test
  (test-true "debug <= debug" (log-level<=? 'debug 'debug))
  (test-true "fatal <= debug" (log-level<=? 'fatal 'debug))
  (test-true "info <= debug" (log-level<=? 'info 'debug))
  (test-true "fatal <= error" (log-level<=? 'fatal 'error))
  (test-true "error <= warning" (log-level<=? 'error 'warning))
  (test-false "info <= error" (log-level<=? 'info 'error))
  (test-false "not debug <= fatal" (log-level<=? 'debug 'fatal)))

(define receiver (make-log-receiver tool-logger 'debug))
(define logging-thread #f)
(define (thunk-for-thread)
  (let loop ()
    (define vec (sync receiver))
    (match vec
      [(and v (vector log-level _ _ _))
       #:when (or (log-level<=? log-level 'error) ; fatal, error always logged.
                  (log-level<=? log-level (*log-level*)))
       (handle-log-message v)]
      [_ (void)])
    (loop)))

;; Was having confusion with using the logging library with an error display
;; handler...
(define (poke)
  (match logging-thread
    [(or #f (? thread-dead?))
     (set! logging-thread (thread thunk-for-thread))]
    [(not (? thread-running?))
     (thread-resume logging-thread)]
    [_
     (void)]))

(define (dispatch-actions log-level logline)
  (for ([action actions])
    (action log-level logline)))

(define (format-logline vec)
  (match-define (vector log-level msg loc sym) vec)
  (define (get-date)
    (if (*log-show-date*)
        (string-append (date->string (current-date) #t) " ")
        ""))
  (define (get-filename)
    (cond [(and (*log-show-filename*)
                loc)
           (match-define (struct srcloc (source line _ _ _)) loc)
           (format " ~a:~a"
                   (match source
                     [(or (? string? s)
                          (and (? path?) (app path->string s)))
                      (basename s)])
                   line)]
          [else ""]))
  (define (get-topic)
    (if (and (*log-show-topic*) sym) (format " [~a]" sym) ""))
  (define (get-message)
    (if sym
        (string-replace msg (format "~a: " sym) "" #:all? #f) ; Delete topic, it's not a feature we want.
        msg))
  (define prefix (parameterize ([date-display-format 'iso-8601])
                   (string-append
                    (get-date)
                    (~a (hash-ref short-names log-level))
                    (get-topic)
                    (get-filename)
                    " ")))
  (string-append
   prefix
   (match (string-split (get-message) #rx"\r\n|\n")
     [(list) "(No message)"]
     [(list a) a]
     [(list a b ...)
      (define padding (make-string (string-length prefix) #\space))
      (string-join (cons a (for/list ([line b])
                             (string-append padding line)))
                   "\n")])))

(define/contract (handle-log-message vec)
  (log-vector? . -> . void?)
  (match-define (vector log-level msg loc sym) vec)
  (dispatch-actions log-level (format-logline vec))
  (when (symbol=? log-level 'fatal)
    (channel-put finished-logging-panic #t)))

(define (log-to-stderr log-level logline)
  (match (hash-ref colors log-level)
    [(list foreground maybe-background-xs ...)
     (parameterize ([foreground-color foreground])
       ((match maybe-background-xs
          [(list background) (λ (proc) (parameterize ([background-color background]) (proc)))]
          [(list) (λ (proc) (proc))])
        (thunk (color-displayln logline (current-error-port)))))]))

(define original-error-display-handler (error-display-handler))

(define-syntax (tool-deinit-logging! stx)
  (syntax/loc stx
    #'(begin
        (error-display-handler original-error-display-handler)
        (set! actions empty)
        (when (thread-running? logging-thread)
          (kill-thread logging-thread)))))

(define DEFAULT-ACTIONS (list log-to-stderr))

(define (tool-init-logging! #:actions [new-actions DEFAULT-ACTIONS])
  (set! actions new-actions)
  (error-display-handler my-error-display-handler)
  (poke))


(define (log-to-syslog log-level logline)
  (syslog (match log-level
            ['debug 'LOG_DEBUG]
            ['info 'LOG_INFO]
            ['warning 'LOG_WARNING]
            ['fatal 'LOG_CRIT]
            ['error 'LOG_ERR])
          logline))

(define actions (list log-to-stderr))

(define (func)
  (debug "You shouldn't see this message")
  (info "Running Racket ~a" (version))
  (err "Something went wrong but I can continue...")
  (panic "oops i died")
  ;; Not reached.
  (warn "Never complained of"))

(module+ main
  ;; (*log-level* 'debug)
  (tool-init-logging!)
  (func))
