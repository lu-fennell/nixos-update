#lang racket

(require racket/date
         "collects/shell/main.rkt" 
         "collects/sudo/main.rkt"
         "parameters.rkt"
         "installed-versions.rkt"
         )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parameters for main app
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mode (make-parameter 'update))
(define rest-args (make-parameter null))
(define verbose? (make-parameter #f))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some command paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-executable-path git)
(define-executable-path mkdir)
(define-executable-path chmod)
(define-executable-path cp)
(define-executable-path rsync)
(define-executable-path nix-env-rebuild)
(define-executable-path nixos-rebuild)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some git tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Output Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (info fmt . args)
  (apply printf (~a "* " fmt "~n") args))

(define (verbose-info fmt . args)
  (when (verbose?)
    (apply info fmt args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update)
  (info "Starting update")
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


  (define (tag-repos tag-prefix nixpkgs config)
    (define hostname (string-trim (with-output-to-string (thunk (system "hostname")))))
    (define stamp
      (~a hostname "-" tag-prefix
          (parameterize ([date-display-format 'iso-8601])
            (string-replace (date->string (current-date) #t) ":" "_"))))
    (info "Build successful. Tagging repos with stamp ~a" stamp)
    (for ([repo (list nixpkgs config)]) (system* git "-C" repo "tag" stamp))
    )
  
  (unless (or 
           (build-system?)
           (build-env?))
    (info "... nothing to do"))

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
    (define commands
      (list "-I" (nixpkgs)
            "-I" (~a "nixpkgs=" (nixpkgs))
            (if (execute?) "switch" "dry-run")))
    (info (apply ~a #:separator " " "Running nixos-rebuild" commands))
    (define rebuild-ok? (apply system*/sudo nixos-rebuild commands))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (command-line
   #:once-any
   [("--env") "Run nix-env with current nixpkgs" (mode 'nix-env)]
   [("--update") "Update the system (default)" (mode 'update)]
   #:once-each
   [("--build-system") "Build the system (when updating)" (build-system? #t)]
   [("--build-env") "Build user environment" (build-env? #t)]
   [("--verbose") "More output" (verbose? #t)]
   #:once-any
   [("--execute") "Switch to new configuration" (execute? #t)]
   #:args args (rest-args args)
   )
  
  
  

  (parameterize ([current-environment-variables (environment-variables-copy (current-environment-variables))])
    (putenv "NIX_PATH" (~a (path->string (nixpkgs))
                           ":"
                           "nixpkgs="
                           (path->string (nixpkgs))
                           ":"
                           (getenv "NIX_PATH")))
    (putenv "NIX_PAGER" "")
    (verbose-info "Setting NIX_PATH to ~a" (getenv "NIX_PATH"))

    (match (mode)
      ['update (update)]
      ['nix-env  (cmd "nix-env" "-f" "<nixpkgs>" (rest-args))]
      [_ (raise-user-error "Unknown command: ~a" (mode))])
    )
  )