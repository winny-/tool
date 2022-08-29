#lang racket/base

(require ffi/unsafe/define
         (rename-in ffi/unsafe (-> ffi->)))

(provide (all-defined-out))

(define-ffi-definer define-libc
  (case (system-type 'os)
    [(unix) (ffi-lib "libc" "6")]
    [(macosx) (ffi-lib "libSystem")]
    [else (raise (exn:fail:unsupported
                  "Unsupported OS does not support syslog library."
                  (current-continuation-marks)))]))

(define _log_level
  (_enum '(LOG_EMERG
           = 0
           LOG_ALERT
           LOG_CRIT
           LOG_ERR
           LOG_WARNING
           LOG_NOTICE
           LOG_INFO
           LOG_DEBUG)))

(define-libc openlog (_fun _string _int _int ffi-> _void))
(define (syslog priority s)
  (define-libc syslog (_cprocedure (list _log_level _string _string)
                                   #:varargs-after 2 _void))
  (syslog priority "%s" s))
(define-libc closelog (_fun ffi-> _void))
