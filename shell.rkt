#lang racket

(provide
 define-executable-path
 ~
 :/
 cmd
 cmd/bool
 cmd/string
 overwrite
 system*/string
 )

(module+ test
  (require rackunit))

;; file system and path utilities
(define ~ (lambda args (apply build-path (find-system-path 'home-dir) args)))
(define :/ (lambda args (apply build-path args)))

(define-syntax-rule (chdir dir-spec body ...)
  (parameterize ([current-directory (if (relative-path? dir-spec)
                                        (build-path (current-directory) dir-spec)
                                        dir-spec)])
    body ...))

;; some command shortcuts
(require (for-syntax syntax/parse))
(define-syntax (define-executable-path stx)
  (syntax-parse stx
    [(_ cmd:id) #`(define cmd (find-executable-path #,(symbol->string
                                                       (syntax-e #'cmd))))]))


(module+ test)

;; some shortcuts to run processes and do io
(define (system*/string cmd . args)
  (with-output-to-string
   (thunk
    (apply system* cmd args))))

(define (system*/port cmd . args)
  (let-values ([(in out) (make-pipe)])
    (apply process*/ports out (current-input-port) (current-error-port)
                    cmd args)
    in))

(define (cmd/bool command . args)
  (define (ensure-string p)
    (cond
      [(path? p) (path->string p)]
      [#t p]))
  (apply system* (find-executable-path command)
         (flatten (map ensure-string args))))

(define (cmd command . args)
  (begin0
      (void)
    (apply cmd/bool command args)))

(define (cmd/string command . args)
  (with-output-to-string
    (thunk (apply cmd command args))))

(require (for-syntax racket/port racket/function))
(define-syntax-rule (overwrite file body ...)
  (with-output-to-file #:exists 'replace file
    (thunk body ...)))


(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
