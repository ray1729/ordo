(define-module (ordo util process)
  #:use-module (ice-9 textual-ports)
  #:export (with-cwd with-env capture))

(define-syntax with-cwd
  (syntax-rules ()
    ((_ new-dir body ...)
     (let ((original-dir (getcwd)))
       (dynamic-wind
         (lambda () (chdir new-dir))
         (lambda () body ...)
         (lambda () (chdir original-dir)))))))

;; Not needed for CAPTURE, which supports an environment override,
;; but might be useful for SYSTEM and SYSTEM*
(define-syntax with-env
  (syntax-rules ()
    ((_ new-env body ...)
     (let ((original-env (environ)))
       (dynamic-wind
         (lambda () (environ new-env))
         (lambda () body ...)
         (lambda () (environ original-env)))))))

;; Run a command and capture the output. Currently this only supports
;; text input and output. If necessary, we could use the (rnrs io ports)
;; module and use PUT-BYTEVECTOR / GET-BYTEVECTOR-ALL and examine the type
;; of STDIN to determine whether to call PUT-STRING or PUT-BYTEVECTOR. For
;; STDOUT, we'd need to add a #:binary argument so the caller could indicate
;; they are expecting binary output. Not implemented yet incase YAGNI.
(define* (capture cmd
                  #:optional (args '())
                  #:key (combine-output #f) (env #f) (stdin #f) (cwd #f))
  (if cwd
      (with-cwd cwd (run cmd args #:combine-output combine-output #:env env #:stdin stdin))
      (let* ((input-pipe (pipe))
             (output-pipe (pipe))
             (pid (spawn cmd (cons cmd args)
                         #:input (car input-pipe)
                         #:output (cdr output-pipe)
                         #:error (if combine-output (cdr output-pipe) (current-error-port))
                         #:environment (or env (environ)))))
        (close-port (cdr output-pipe))
        (close-port (car input-pipe))
        (when stdin (put-string (cdr input-pipe) stdin))
        (close-port (cdr input-pipe))
        (let ((output (get-string-all (car output-pipe))))
          (close-port (car output-pipe))
          (values (cdr (waitpid pid)) output)))))
