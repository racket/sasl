#lang racket/base

(require net/base64
         racket/match)

(provide (all-defined-out))

;; ----

(struct sasl-ctx (outbox k) #:mutable)
;; where outbox : String/Bytes or #f -- #f means no message available
;;       k : (self String/Bytes -> Void) | 'done | 'error
;; INV: if k = 'error then outbox = #f

(define (make-sasl-ctx aux out next)
  (sasl-ctx out (wrap-next-proc aux next)))

(define (sasl-next-message ctx)
  (define msg (sasl-ctx-outbox ctx))
  (unless msg
    (error 'sasl-next-message
           "sequence violation: not allowed in current state\n  state: ~s\n  allowed in: ~s"
           (sasl-state ctx) '(send/receive send/done)))
  msg)

(define (sasl-receive-message ctx msg)
  (match (sasl-ctx-k ctx)
    [(? procedure? recv-k) (recv-k ctx msg)]
    [_ (error 'sasl-receive-message
              "sequence violation: not allowed in current state\n  state=~s\n  allowed in: ~s"
              (sasl-state ctx) '(receive send/receive))]))

(define (sasl-state ctx)
  (match (sasl-ctx-k ctx)
    [(? procedure?) (if (sasl-ctx-outbox ctx) 'send/receive 'receive)]
    ['done (if (sasl-ctx-outbox ctx) 'send/done 'done)]
    ['error 'error]))

;; ----

(define (set-sasl! ctx outbox k)
  (set-sasl-ctx-outbox! ctx outbox)
  (set-sasl-ctx-k! ctx k))

(define (wrap-next-proc aux proc)
  (if (eq? proc 'done)
      'done
      (lambda (ctx data)
        (unless (or (bytes? data)
                    (string? data))
          (raise-argument-error 'sasl-receive-message "(or/c bytes? string?)" data))
        (with-handlers ([exn:fail? (λ (e) (fatal ctx "custom context error: ~a" (exn-message e)))])
          (define-values (out next)
            (proc aux data))
          (set-sasl! ctx out (wrap-next-proc aux next))))))

;; ----------------------------------------

(struct exn:fail:sasl:fatal exn:fail (msg))

(define (fatal ctx #:who [who0 #f] fmt . args)
  (when ctx
    (set-sasl-ctx-k! ctx 'error)
    (set-sasl-ctx-outbox! ctx #f))
  (define who (or who0 'sasl-receive-message))
  (define msg (apply format fmt args))
  (raise (exn:fail:sasl:fatal (format "~a: ~a" who msg) (current-continuation-marks) msg)))

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
