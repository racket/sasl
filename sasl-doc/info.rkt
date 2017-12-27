#lang info

(define version "1.0")
(define collection "sasl")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "sandbox-lib"
                     "racket-doc"))
(define update-implies '("sasl-lib"))
