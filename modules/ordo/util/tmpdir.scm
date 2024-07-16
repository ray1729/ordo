(define-module (ordo util tmpdir)
  #:use-module (system foreign)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format)
  #:export (create-temporary-directory))

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
;; TODO: we also need to borrow their delete-file-recursively
;; (define (call-with-temporary-directory proc)
;;   "Call PROC with a name of a temporary directory; close the directory and
;; delete it when leaving the dynamic extent of this call."
;;   (let* ((directory (or (getenv "TMPDIR") "/tmp"))
;;          (template  (string-append directory "/ordo.XXXXXX"))
;;          (tmp-dir   (mkdtemp! template)))
;;     (dynamic-wind
;;       (const #t)
;;       (lambda ()
;;         (proc tmp-dir))
;;       (lambda ()
;;         (false-if-exception (delete-file-recursively tmp-dir))))))
