#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#
(use-modules (srfi srfi-11)
             (ice-9 getopt-long)
             (ice-9 format)
             (ordo util tmpdir))

(define (collect-files tarball dir . files)
  (let* ((mode (if (file-exists? tarball) "--append" "--create"))
         (rc (apply system* "tar" mode "--gzip" "--directory" dir "--file" tarball files)))
    (unless (zero? rc)
      (error (string-append "Error collecting files")))))

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
    (values (car args) target)))

(define (main args)
  (let-values (((playbook-path target) (process-options args)))
    (define playbook (load (canonicalize-path playbook-path)))
    (define top-dir (dirname (dirname (current-filename))))
    (define tmp-dir (create-temporary-directory))
    (define tarball (string-append tmp-dir "/payload.tar.gz"))
    (collect-files tarball top-dir "modules" "bin")))
