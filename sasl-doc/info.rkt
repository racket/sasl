#lang info

(define collection "sasl")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     ["sasl-lib" #:version "1.1"]
                     "racket-doc"))
(define update-implies '("sasl-lib"))

(define scribblings '(("sasl.scrbl" () (net-library))))

(define pkg-authors '(ryanc))
