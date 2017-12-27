#lang racket/base
(require racket/match
         racket/string
         racket/list
         net/base64)
(provide (all-defined-out))

;; ----

(struct sasl-ctx (outbox k) #:mutable)
;; where outbox : String/Bytes or #f -- #f means no message available
;;       k : (self String/Bytes -> Void) | (cons 'send k) | 'success | 'error

(define (sasl-next-message ctx)
  (define msg (sasl-ctx-outbox ctx))
  (unless msg (error 'sasl-next-message "no outgoing message available"))
  (match (sasl-ctx-k ctx)
    [(cons 'send k) (set-sasl-ctx-k! ctx k)]
    [_ (void)])
  msg)

(define (sasl-receive-message ctx msg)
  (match (sasl-ctx-k ctx)
    [(? procedure? recv-k) (recv-k ctx msg)]
    [(cons 'send k) (bad-state 'sasl-receive-message "call sasl-next-message first")]
    ['success (bad-state 'sasl-receive-message "protocol finished")]
    ['error (bad-state 'sasl-receive-message "authentication process failed")]))

(define (sasl-succeeded? ctx)
  (match ctx [(sasl-ctx out 'success) #t] [_ #f]))
(define (sasl-failed? ctx)
  (match ctx [(sasl-ctx out 'error) #t] [_ #f]))

;; ----
(define (bad-state who what)
  (error who "sequence violation;\n ~a" what))

(define (set-sasl! ctx outbox k)
  (set-sasl-ctx-outbox! ctx outbox)
  (set-sasl-ctx-k! ctx k))

(define (with-ctx ctx proc)
  (with-handlers ([void (lambda (e) (set-sasl-ctx-k! ctx 'error) (raise e))])
    (proc)))

;; ----------------------------------------

(struct exn:fail:sasl:fatal (msg))

(define (fatal ctx #:who [who #f] fmt . args)
  (when ctx (set-sasl-ctx-k! ctx 'error))
  (define who (or who 'sasl-receive-message))
  (define msg (apply format fmt args))
  (raise (exn:fail:sasl:fatal (current-continuation-marks) (format "~a: ~a" who msg) msg)))

;; ----------------------------------------

(define (->base64-bytes s [nl #""])
  (base64-encode (if (string? s) (string->bytes/utf-8 s) s) nl))
(define (->base64-string s [nl #""])
  (bytes->string/utf-8 (->base64-bytes s nl)))

;; FIXME: net/base64's decoder skips invalid chars; should validate encoding
(define (base64->bytes s)
  (base64-decode (if (string? s) (string->bytes/utf-8 s) s)))
(define (base64->string s)
  (bytes->string/utf-8 (base64->bytes s)))
