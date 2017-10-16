#lang info
(define collection "ssm")
(define deps '("beautiful-racket-lib"
               "br-parser-tools-lib"
               "brag"
               "base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/simple-stack-machine.scrbl" (multi-page))))
(define pkg-desc "A simple stack machine.")
(define version "0.1")
(define pkg-authors '(basty))
