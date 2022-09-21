#lang debug super racket

(require (for-syntax syntax/parse racket/string racket/syntax)
         syntax/parse racket/string
         data/either
         (prefix-in : data/functor)
         dirname
         tool)

(tool-init!)
#|
Some tradeoffs of command-line:

1. It already exists.

2. It doesn't offer any way to validate parameters or arguments (e.g. validate
if an argument is a number and a port number).

3. No way to do subcommands without chaining and verbose syntax.

4. It's not easy to modify, uses some macro techniques that are simpler, but
harder to work with.

5. It doesn't feel intuitive to use.  It doesn't feel nice to use.

|#

(begin-for-syntax
  (define-splicing-syntax-class body
    (pattern (b*:expr ...+)
             #:attr begin #'(begin b* ...)))
  (define-syntax-class flag
    (pattern (flags:id ...+ help:string type:expr)))
  (define-syntax-class arg
    (pattern (binding:id help:string type:expr)
             #:attr arg #'binding))
  (define-syntax-class subcommand
    (pattern (name:id
              (~optional (~seq #:help help0:string help*:string ...))
              (~optional (~seq #:flags flag0:flag flag*:flag ...))
              (~optional (~seq #:args arg0:arg arg*:arg ...))
              (~optional (~seq #:body b0:expr b*:expr ...))
              (~optional (~seq #:subcmds cmd0:subcommand cmd*:subcommand ...))))))

(struct FlagType (name arg? validate) #:prefab)
(define-syntax (define-flag-type stx)
  (syntax-parse stx
    [(_ name:id arg?:boolean validate:expr)
     (with-syntax ([flag:name (format-id #'name "flag:~a" (syntax-e #'name))])
       #'(begin
           (define flag:name (FlagType 'name arg? validate))
           (provide flag:name)))]))
(define-flag-type bool #f (const (success #t)))
(define-flag-type arg-bool #t
  (λ (s)
    (match (string-downcase (string-trim s))
      [(pregexp #px"y|ye|yes") (success #f)]
      [(pregexp #px"n|no") (success #f)]
      [s (failure (format "Could not parse \"~a\" as a boolean string (YES|NO)." s))])))
(define-flag-type int #t
  (match-lambda
    [(and (app string->number n) n) (success n)]
    [s (failure (format "Could not parse \"~a\" as a number." s))]))
(define-flag-type string #t success)
(define-flag-type port-number #t
  (match-lambda
    [(and (app string->number n) (? port-number? n)) (success n)]
    [s (failure (format "Could not parse \"~a\" as a port number." s))]))

(define *exit-on-bad-usage?* (make-parameter #f))

(define argument%
  (class object%
    (super-new)
    (init-field flag-type help)
    (match-define (struct* FlagType ([name name]
                                     [arg? arg?]
                                     [validate my-validate]))
      flag-type)
    (define/public (get-arg?) arg?)
    (define/public (get-type-name) name)
    (define/public (validate s) (my-validate s))
    (define/public (get-help) help)))
(define flag-argument%
  (class argument%
    (super-new)
    (init-field flags)
    (define/public (get-flags) flags)
    (define/public (custom-display op)
      (printf "flag-argument% flags=~a" flags))))
(define arg-argument%
  (class* argument% ()
    (super-new)
    (init-field binding)
    (define/public (get-binding) binding)
    (define/public (custom-display op)
      (printf "arg-argument% binding=~a" binding))))

(struct ParseResult (bindings flags) #:transparent)
(define (flag-arg? s)
  (string-prefix? s "-"))

(define cmdline%
  (class object%
    (super-new)
    (init-field help flags args body subcmds)

    (define flags+object (for*/hash ([flag flags]
                                     [-flag (flag.get-flags)])
                           (values -flag
                                   flag)))

    (define/public (program)
      (basename (find-system-path 'run-file)))

    (define/public (run [args (current-command-line-arguments)])
      (parse args))

    (define/public (parse cmd-args)
      (let loop ([acc (ParseResult (hash) (hash))] [args (vector->list cmd-args)] [stack args])
        (match-define (struct ParseResult (acc-bindings acc-flags)) acc)
        (define (validate argument val)
          (match (argument.validate val)
            [(failure s) (panic "~a" s)]
            [(success v) v]))
        (match #R args
               [(list (? flag-arg? flag) args ...)
                (printf "flag-arg ~a" flag)
                (match (hash-ref flags+object (string->symbol flag) #f)
                  [#f (panic "Unknown parameter \"~a\" with remaining parameters \"~a\""
                             flag args)]
                  [🏁
                   (define (recur val args)
                     (loop (ParseResult acc-bindings (cons (cons 🏁
                                                                 (validate 🏁 val))
                                                           acc-flags))
                           args
                           stack))
                   (match* [(🏁.get-arg?) args]
                     [(#t (list))
                      (panic "Not enough arguments.")]
                     [(#f _)
                      (recur #t args)]
                     [(#t (list v args2 ...))
                      (recur v args2)])])]
               [(list arg args ...)
                (match stack
                  [(list) (panic "Too many arguments.")]
                  [(list spec specs ...)
                   (loop (ParseResult (cons (cons spec (spec.validate arg)) acc) acc-flags)
                         args
                         specs)])]
               [(list)
                #R acc])))

    (define/public (usage)
      (define s-args (string-join (for/list ([arg args]) (~a (arg.get-binding)))))
      (printf "~a: ~a\n\n" (program) s-args)
      (printf "~a\n\n" (string-trim (string-join help "\n\t")))
      (for ([flag flags])
        (printf "~a ~a\n"
                (string-join (map ~a (flag.get-flags)) ", ")
                (if (flag.get-arg?)
                    (format "~a (~a)" 'X (flag.get-type-name))
                    "(Boolean option - rubout to disable)")))
      (when (*exit-on-bad-usage?*)
        (exit 1)))))

(define-syntax (cmdline stx)
  (syntax-parse stx
    [(_
      (~optional (~seq #:help help*:string ...+))
      (~optional (~seq #:flags flag*:flag ...+))
      (~optional (~seq #:args arg*:arg ...+))
      (~optional (~seq #:body b*:expr ...+))
      (~optional (~seq #:subcmds cmd*:subcommand ...+)))
     (with-syntax ([help-list (if (attribute help*)
                                  #'(list help* ...)
                                  #'(list "A Racket cli tool"))]
                   [body-begin (if (attribute b*)
                                   #'(begin b* ...)
                                   #'(void))]
                   [args-list (if (attribute arg*)
                                  #'(list (new arg-argument%
                                               [help arg*.help]
                                               [flag-type arg*.type]
                                               [binding 'arg*.binding]) ...)
                                  #'(list))]
                   [flags-list (cond
                                 [(attribute flag*)
                                  #'(list (new flag-argument%
                                               [help flag*.help]
                                               [flag-type flag*.type]
                                               [flags '(flag*.flags ...)])
                                          ...)]
                                 [else #'(list)])])
       #`(send (new cmdline%
                    [help help-list]
                    [flags flags-list]
                    [args args-list]
                    [body (λ () body-begin)]
                    [subcmds (list)])
               run))]))


(cmdline
 #:help
 "help msg"
 #:flags
 (-v --verbose "" flag:bool)
 (-d --debug "debug" flag:bool)
 #:args
 (a "thing" flag:string) (b "other" flag:int) (c "what" flag:arg-bool)
 #:body
 2
 #:subcmds
 (status
  #:help
  "It does a thing"
  #:body
  1
  )
 )
