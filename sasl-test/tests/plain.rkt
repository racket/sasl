#lang racket/base
(require rackunit
         sasl
         sasl/plain)

(check-equal? (plain-client-message "tim" "tanstaaftanstaaf")
              "\0tim\0tanstaaftanstaaf")
