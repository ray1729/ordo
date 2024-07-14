(define-module (ordo task command)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-11)
  #:use-module (ordo task)
  #:use-module (ordo util run)
  #:export (task-command))


(define* (task-command name cmd #:optional (args '())
                       #:key (fail-ok? #f) (stdin #f) (cwd #f) (env #f))
  (make-task name
             #f
             (lambda ()
               (let-values (((exit-code output) (run cmd args #:stdin stdin #:cwd cwd #:env env #:combine-output #t)))
                 (if (or fail-ok? (zero? exit-code))
                     (values exit-code output)
                     (error (format #f "Error running ~a (exit ~d): ~a" cmd exit-code output)))))))
