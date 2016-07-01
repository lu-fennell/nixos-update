#lang racket

(require shell
         sudo
         racket/date
         "parameters.rkt"
         "installed-versions.rkt"
         )


(command-line
 #:once-each
 [("--build-system") "Build the system" (build-system? #t)]
 [("--build-env") "Build user environment" (build-env? #t)]
 #:once-any
 [("--execute") "Switch to new configuration" (execute? #t)]
 )
 
;; Define two commands for convenience
(define-executable-path nix-env-rebuild)
(define-executable-path nixos-rebuild)
 
 
 ;; check if repos are all clean
 (for ([d (list (nixpkgs)
                (nixos-config)
                (nix-env-config))])
      (let ([status-output (git-status d)])
        (unless (git-status-output-clean? status-output)
          (raise-user-error 'clean-check "aborting, as git repo ~a is dirty:~n~a" d status-output))
        ))


(system* mkdir "-p" (nixos-out-dir))
(define (write-version-script script-file title cfg)
  
  (overwrite script-file
           (displayln (installed-version-sh title cfg)))
  (system* chmod "a+x" script-file))

;; build system
(define (info fmt . args)
  (apply printf (~a "* " fmt "~n") args))

(define (tag-repos tag-prefix nixpkgs config)
  (define hostname (string-trim (with-output-to-string (thunk (system "hostname")))))
  (define stamp
    (~a hostname "-" tag-prefix
        (parameterize ([date-display-format 'iso-8601])
          (string-replace (date->string (current-date) #t) ":" "_"))))
  (info "Build successful. Tagging repos with stamp ~a" stamp)
  (for ([repo (list nixpkgs config)]) (system* git "-C" repo "tag" stamp))
  )

(parameterize ([current-environment-variables (environment-variables-copy (current-environment-variables))])
  (putenv "NIX_PATH" (~a (path->string (nixpkgs)) ":" (getenv "NIX_PATH")))
  (info "Setting NIX_PATH to ~a" (getenv "NIX_PATH"))
 (when (build-system?)
   (displayln "Building system...")
   (info "Installing version info to ~a" (path->string (nixos-out-dir)))
   (write-version-script (:/ (nixos-out-dir) "nixos-installed-version") "nixos" (nixos-config))
   ;; TODO: clean out dir first
   (info "Caching system config to ~a" (path->string (nixos-out-dir)))
   (for ([f (in-directory (nixos-config))]
         #:when (equal? (filename-extension f) #"nix"))
        (copy-file f (:/ (nixos-out-dir) (find-relative-path (nixos-config) f)) #t))
   ;; TODO: validate for errors first (using nix-shell or something)
   (info "Copying system config to ~a" (nixos-dest-dir))
   (system*/sudo rsync "-rv" (~a (path->string (nixos-out-dir)) "/") (path->string (nixos-dest-dir)))
   (define command (if (execute?) "switch" "dry-run"))
   (info "Running nixos-rebuild ~a" command)
   (define rebuild-ok? (system*/sudo nixos-rebuild command))
   (when (and rebuild-ok? (execute?))
     ;; TODO tag w/ the hostname
     (tag-repos "system-build-success-" (nixpkgs) (nixos-config)))
   
   )
 
 ;; build env
 (when (build-env?)
   (displayln "Building environment...")
   (printf "Copying user config to ~a~n" (path->string (nix-env-config)))
   (write-version-script (:/ (nix-env-config) "nix-env-installed-version" "nix-env-installed-version")
                         "nix-env" (nix-env-config))
   (define command (if (execute?) "switch" "dry-run"))
   (printf "Running nix-env-rebuild ~a~n" command)
   (define rebuild-ok? (system* nix-env-rebuild command))
   (when (and rebuild-ok? (execute?))
     (tag-repos "env-build-success-" (nixpkgs) (nix-env-config))))
   )
