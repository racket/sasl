#lang racket/base
(require rackunit
         sasl/saslprep)

;; stringprep examples (bidi)
(check-exn #rx"saslprep: bidirectional check failed"
           (lambda () (saslprep "\u0627\u0031")))
(check-equal? "\u0627\u0031\u0628"
              "\u0627\u0031\u0628")

;; saslprep examples
(check-equal? (saslprep "I\u00ADX") "IX")
(check-equal? (saslprep "user")     "user")
(check-equal? (saslprep "USER")     "USER")
(check-equal? (saslprep "\u00AA")   "a")
(check-equal? (saslprep "\u2168")   "IX")
(check-exn #rx"saslprep: prohibited character in string"
           (lambda () (saslprep "\u0007")))
(check-exn #rx"saslprep: bidirectional check failed"
           (lambda () (saslprep "\u0627\u0031")))
