#lang racket

(provide
 list-nix-channels)

(define (list-nix-channels channels-dir)
  (if (directory-exists? channels-dir)
      (for/set ([f (directory-list channels-dir)]
                #:when (not (set-member? (map string->path
                                            '("manifest.nix"
                                              "binary-caches")) f))
                ) f)
      (set)))