#lang info
(define collection "nixos-update")
(define deps '("base"
               "rackunit-lib"
	       "sudo"
	       ))
(define build-deps '("scribble-lib"
		     "racket-doc"
		     ))
(define scribblings '(("scribblings/nixos-update.scrbl" ())))
(define pkg-desc "Script for a managed nixos installation.")
(define version "0.0")
(define pkg-authors '(fennell))
