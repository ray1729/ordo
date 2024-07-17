(define-module (ordo task command)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-11)
  #:use-module (ordo task)
  #:use-module (ordo util process)
  #:export (command))

(define* (command name cmd #:optional (args '())
                  #:key (fail-ok? #f) (stdin #f) (cwd #f) (env #f) (skip? #f))
  (make-task name
             '()
             skip?
             (lambda ()
               (let-values (((exit-code output) (run cmd args #:stdin stdin #:cwd cwd #:env env #:combine-output #t)))
                 (if (or fail-ok? (zero? exit-code))
                     (values exit-code output)
                     (error (format #f "Error running ~a (exit ~d): ~a" cmd exit-code output)))))))
