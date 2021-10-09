#lang info

(define collection "sasl")
(define deps '("base"
               ["sasl-lib" #:version "1.1"]
               "rackunit-lib"))
(define update-implies '("sasl-lib"))

(define pkg-authors '(ryanc))

(define license
  '(Apache-2.0 OR MIT))
