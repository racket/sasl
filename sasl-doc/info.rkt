#lang info

(define version "1.0")
(define collection "sasl")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "sasl-lib"
                     "racket-doc"))
(define update-implies '("sasl-lib"))

(define scribblings '(("sasl.scrbl" () (net-library))))

(define pkg-authors '(ryanc))
