#lang racket/base
(require rackunit
         sasl/private/intset)

(define (int-in-set?/linear n is)
  (for/or ([i (in-range 0 (vector-length is) 2)])
    (<= (vector-ref is i) n (vector-ref is (add1 i)))))

(let ()
  (define v '#(2 5  8 8  12 12  13 13  19 20  25 29))
  (for ([i (in-range 32)])
    (check-equal? (int-in-set?/linear i v)
                  (int-in-set? i v))))

(let ()
  (define v #(2 4  7 7  9 9  15 19  21 21))
  (for ([i (in-range 25)])
    (check-equal? (int-in-set? i v)
                  (int-in-set?/linear i v))))
