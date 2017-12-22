#lang racket/base
(require racket/match
         racket/string)

;; References:
;; - https://tools.ietf.org/html/rfc4616

;; message   = [authzid] UTF8NUL authcid UTF8NUL passwd
;; authcid   = 1*SAFE ; MUST accept up to 255 octets
;; authzid   = 1*SAFE ; MUST accept up to 255 octets
;; passwd    = 1*SAFE ; MUST accept up to 255 octets
;; UTF8NUL   = %x00   ; UTF-8 encoded NUL character
;; SAFE      = ...    ; any UTF-8 encoded Unicode character except NUL

(define name "PLAIN")

(define (safe-char? c) (not (eqv? c #\nul)))
(define (safe-string? s) (for/and ([c (in-string s)]) (safe-char? c)))

;; ENCODE

;; encode-sasl-plain : String String/#f String -> String
(define (encode-sasl-plain authcid authzid passwd)
  (string-append (or authzid "") "\0" authcid "\0" passwd))

;; DECODE

;; decode-sasl-plain : String -> (values String String/#f String)
(define (decode-sasl-plain message)
  (define (bad msg) (error 'decode-sasl-plain "~a\n  message: ~e" msg message))
  (match (string-split message "\0" #:trim? #f)
    [(list authz authc passwd)
     (when (zero? (string-length authc)) (bad "empty decoded authcid"))
     (when (zero? (string-length passwd)) (bad "empty decoded password"))
     (values authc (and (positive? (string-length authz)) authz) passwd)]
    [_ (bad "ill-formed message")]))
