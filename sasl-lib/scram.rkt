#lang racket/base
(require racket/contract/base
         "private/base.rkt"
         "private/scram.rkt")
(provide (contract-out
          [make-scram-client-ctx
           (->* [(or/c 'sha1 'sha256 'sha512)
                 string?
                 string?]
                [#:authorization-id (or/c #f string?)
                 #:channel-binding (or/c boolean? (list/c symbol? bytes?))]
                sasl-ctx?)]))
