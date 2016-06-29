#lang racket

(require "shell.rkt"
         "parameters.rkt"
         )

(provide (all-defined-out))

;; create nixos-installed-nixpkgs-version and nix-env-installed-nixpkgs-version
(define (get-version dir)
  (system*/string git "-C" dir "log" "-n" "1"))

(define (installed-version-text cfg pkgs)
  (format #<<END
Config:
-------
~a

Nixpkgs:
--------
~a
END
          (get-version cfg)
          (get-version pkgs)))

(define (installed-version-sh title cfg)
  (format #<<END
#!/bin/sh
cat <<EOF
~a versions
==============
~a
EOF
END
          title
          (installed-version-text cfg (nixpkgs))))
