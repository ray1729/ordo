(define-module (ordo util filesystem)
  #:use-module (system foreign)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export (delete-file-recursively
            create-temporary-directory
            call-with-temporary-directory))

(define* (delete-file-recursively filename #:key (verbose #f))
  (define dev (stat:dev (stat filename)))
  (define (enter? name stat result)
    (= (stat:dev stat) dev))
  (define (leaf name stat result)
    (if (false-if-exception (delete-file name))
        (and verbose (format #t "delete-file ~a OK~%" name))
        (format (current-error-port) "warning: delete-file ~a failed~%" name))
    result)
  (define (down name stat result)
    result)
  (define (up name stat result)
    (if (false-if-exception (rmdir name))
        (and verbose (format #t "rmdir ~a OK~%" name))
        (format (current-error-port) "warning: rmdir ~a failed~%" name))
    result)
  (define (skip name state result)
    result)
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)
  (file-system-fold enter? leaf down up skip error #f filename))


;; This is based on reading guix/build/syscalls.scm but less general
;; than their implementation.
(define mkdtemp!
  (let* ((ptr (dynamic-func "mkdtemp" (dynamic-link)))
         (proc (pointer->procedure '* ptr '(*) #:return-errno? #t)))
    (lambda (tmpl)
      (let-values (((result err) (proc (string->pointer tmpl))))
        (when (null-pointer? result)
          (error (format #f "mkdtemp! ~a: ~a" tmpl (strerror err))))
        (pointer->string result)))))

(define (create-temporary-directory)
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template (string-append directory "/ordo.XXXXXX")))
    (mkdtemp! template)))

;; This is borrowed from guix/util.scm
(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/ordo.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))
