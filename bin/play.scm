#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#
(use-modules (srfi srfi-11)
             (ice-9 getopt-long)
             (ice-9 format)
             (ordo util filesystem))

(define (tar . args)
  (unless (zero? (apply system* "tar" args))
    (error (format #f "Non-zero exit from tar ~a" args))))

(define* (usage #:optional errmsg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (when errmsg
        (format #t "Error: ~a~%~%" errmsg))
      (display "Usage: play -t TARGET PLAYBOOK")
      (newline)))
  (exit (if errmsg EXIT_FAILURE EXIT_SUCCESS)))

(define (process-options args)
  (let* ((option-spec '((help   (single-char #\h) (value #f))
                        (target (single-char #\t) (value #t) (required? #t))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (target (option-ref options 'target #f))
         (args   (option-ref options '() '())))
    (cond
     (help-wanted (usage))
     ((not (= 1 (length args)))
      (usage "Expected exactly one playbook")))
    (values (canonicalize-path (car args)) target)))

(define (main args)
  (let-values (((playbook-path target) (process-options args)))
    (define playbook (load playbook-path))
    (define top-dir (dirname (dirname (current-filename))))
    (call-with-temporary-directory
     (lambda (tmp-dir)
       (define tarball (string-append tmp-dir "/payload.tar"))
       (tar "--create" "--file" tarball "--directory" top-dir "modules" "bin")
       (tar "--append" "--file" tarball "--transform" "s/.*/playbook.scm/" playbook-path)
       (tar "tf" tarball)))))
