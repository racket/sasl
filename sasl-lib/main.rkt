#lang racket/base
(require racket/contract/base
         "private/base.rkt")
(provide sasl-ctx?
         (contract-out
          [sasl-next-message    (-> sasl-ctx? any/c)]
          [sasl-receive-message (-> sasl-ctx? any/c any)]
          [sasl-succeeded?      (-> sasl-ctx? boolean?)]
          [sasl-failed?         (-> sasl-ctx? boolean?)])
         (struct-out exn:fail:sasl:fatal))
