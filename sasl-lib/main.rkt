#lang racket/base
(require racket/contract/base
         "private/base.rkt")
(provide sasl-ctx?
         sasl-next/c
         (contract-out
          [make-sasl-ctx        (-> any/c (or/c #f bytes? string?) sasl-next/c sasl-ctx?)]
          [sasl-next-message    (-> sasl-ctx? any/c)]
          [sasl-receive-message (-> sasl-ctx? any/c any)]
          [sasl-state           (-> sasl-ctx? symbol?)])
         (struct-out exn:fail:sasl:fatal))

(define sasl-next/c
  (or/c 'done (-> any/c (or/c bytes? string?)
                  (values (or/c #f bytes? string?)
                          (recursive-contract sasl-next/c)))))
