#lang racket

(require "collects/shell/main.rkt")

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;
;; Parameters
;;;;;;;;;;;;;;;;;;;;
; TODO: set this from NIX_PATH
(define nixpkgs (make-parameter
                 (~  "nixos-update/channels/nixos-16.09")))
(define nixos-config (make-parameter
                       (~ "nixos-config")))
(define nix-env-config (make-parameter
                     (~ ".nixpkgs")))
(define nixos-out-dir (make-parameter
                 (~ "nixos-update/_etc_nixos")))
(define nixos-dest-dir (make-parameter
                        (string->path "/etc/nixos")))

(define build-system? (make-parameter #f))
(define build-env? (make-parameter #f))
