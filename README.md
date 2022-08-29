# tool - Opinionated framework for writing tools in Racket

## Features

- Easier logging
- Easier command execution
- Easier path construction
- Whatever is needed to make it work.

### Logging

![Logging example screenshot](./screenshots/logging.png)

- Colors by default
- Show exact source location where the log call was made.
- Show time & hide the topic
- Error handler that logs to the same loggers.  (HACK - Racket doesn't seem to
  support this directly.)
- Simpler API:
  + Just run `(tool-init!)` or `(tool-init-logging!)` on startup to attach loggers.
  + `panic`, `err`, `warn`, `info`, `debug` syntaxes correspond to sending a
    log line with the `fatal`, `error`, `warning`, `info`, `debug` log-levels.
    Usage: `(info "some message string on Racket ~a" (version))`.  Note must
    always contain a format string.  There is no shorthand.  This is
    intentional.
  + `panic` also causes racket to exit with a failure code.¤
  + A different Racket thread logs all messages made with this logger.  By
    default (e.g. initializing with `(tool-init!)`) messages are logged only to
    stderr.  Try `(tool-init! #:logger (list log-to-syslog))` to log to syslog
    instead.  All logging to this logger uses the tool-specific parameter
    `*log-level*` to determine which log levels to print logs for.

See [`./src/tool-examples/examples/logging.rkt`](./src/tool-examples/examples/logging.rkt).

### Shell

Convenience wrappers around run-shell-pipeline.  `$` and `$/string`.

Example:

```racket
;; Run command with STDIN closed, STDOUT and STDIN go to system STDOUT/STDIN
($ '(echo hello world))  ; returns an execution object created by run-shell-pipeline.
hello world
#<pipeline:success=#t,return=0>

;; Raise an exception by default (TODO add backtrace woops)
($ '(false))
pipeline "false" failed with 1

;; Get output to string
(values-second ($/string '("uptime")))
"20:33:46  up 2 days  6:50,  1 user,  load average: 0.20, 0.12, 0.09"
```

Look for `#:out`, `#:error`, `#:in`, `#:raise?` keyword arguments in the
definition of the above functions for further usage.

### Path construction

- `//` create an absolute path
- `./` create a relative path
- `~` create a path rooted in `$HOME`.

### Extras

- `values-first`, `values-second`, ..., `values-ninth`.  Get the nth value from an expression.

## TODO

- Git style subcommands
- `#lang` to simplify command authoring
- Generalizable log routing
- `log-to-console!` to toggle logging to console imperatively.
- `log-to-file!` to toggle logging to file imperatively.
- Option to disable/customize colors

## License

ISC.  See [LICENSE](./LICENSE).
