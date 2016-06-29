#lang racket
(provide ~
         :/
         chdir
         chmod
         cp
         git
         git-status
         git-status-output-clean?
         mkdir
         nix-env-rebuild
         nixos-rebuild
         rsync
         overwrite
         system*/string)

;; file system and path utilities
(define ~ (lambda args (apply build-path (find-system-path 'home-dir) args)))
(define :/ (lambda args (apply build-path args)))

(define-syntax-rule (chdir dir-spec body ...)
  (parameterize ([current-directory (if (relative-path? dir-spec)
                                        (build-path (current-directory) dir-spec)
                                        dir-spec)])
    body ...))

;; some command shortcuts
(define git (find-executable-path "git"))
(define mkdir (find-executable-path "mkdir"))
(define chmod (find-executable-path "chmod"))
(define cp (find-executable-path "cp"))
(define rsync (find-executable-path "rsync"))
(define nixos-rebuild (find-executable-path "nixos-rebuild"))
(define nix-env-rebuild (find-executable-path "nix-env-rebuild"))

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

(require (for-syntax racket/port racket/function))
(define-syntax-rule (overwrite file body ...)
  (with-output-to-file #:exists 'replace file
    (thunk body ...)))

;; git utilities
(define (git-status-output-clean? status-output)
  (for/and ([status-string (in-lines (open-input-string status-output))])
           (or
            (string-prefix? status-string "  ")
            ;;(string-prefix? status-string "??")
            )))

(define (git-status dir)
  (with-output-to-string
    (thunk
     (unless (system* git "-C" dir "status" "--porcelain")
       (raise-user-error 'git-status "failed getting status of ~a" dir)))))
