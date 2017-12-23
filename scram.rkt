#lang racket/base
(require racket/string
         racket/random
         racket/list
         net/base64
         "saslprep.rkt"
         "private/crypto.rkt")
(provide (all-defined-out))

;; base64: canonical, no whitespace!
;; SASLprep in stored mode (ie, disallow unassigned)
;;   client prepares before sending to server

;; parameter H (hash function)

;; Hi(str, salt, i) = PBKDF2[prf=HMAC[H]](str, salt, i)

;; type CBind = String* | #t | #f

;; 1: C->S

(define (make-scram-ctx digest authcid authzid password)
  (define cbind #f)
  (define p-authcid (saslprep authcid))
  (define p-authzid (and authzid (saslprep authzid)))
  (define p-password (string->bytes/utf-8 (saslprep password)))
  (define gs2-header
    (string-append (cond [(string? cbind) (format "p=~a" cbind)]
                         [else (if cbind "y" "n")])
                   "," (if p-authzid (format "a=~a" (encode-name p-authzid)) "") ","))
  (define client-nonce (generate-client-nonce))
  (define msg-c1/bare (format "n=~a,r=~a" (encode-name p-authcid) client-nonce))
  (define msg-c1 (string-append gs2-header msg-c1/bare))
  (hasheq 'digest digest 'authcid authcid 'authzid authzid 'password password
          'p-authcid p-authcid 'p-authzid p-authzid 'p-password p-password
          'cbind cbind 'gs2-header gs2-header
          'client-nonce client-nonce
          'msg-c1/bare msg-c1/bare 'msg-c1 msg-c1))

(define (scram-client-first-message ctx)
  (hash-ref ctx 'msg-c1))

(define CLIENT-NONCE-SIZE 24)
(define (generate-client-nonce)
  ;; How long? SCRAM example has 24 chars.
  ;; Must be printable: %x21-2B / %x2D-7E
  (define out (open-output-string))
  ;; filter, keep printable chars (but not #\,)
  (for ([b (in-bytes (crypto-random-bytes (* 2 CLIENT-NONCE-SIZE)))])
    (let ([b (bitwise-and #x7F b)])
      (when (or (<= #x21 b #x2B) (<= #x2D b #x7E))
        (write-byte b out))))
  (define r (get-output-string out))
  (cond [(>= (string-length r) CLIENT-NONCE-SIZE)
         (substring r 0 CLIENT-NONCE-SIZE)]
        [else ;; unlikely!
         (generate-client-nonce)]))

;; 2: S->C

;; scram-receive-server-first-message : ScramCtx String -> (values Nat String String)
(define (scram-receive-server-first-message ctx msg-s1)
  (define records (split-message msg-s1))
  (define iters0 (hash-ref records #\i))
  (unless (regexp-match? #rx"^[0-9]+$" iters0)
    (error 'scram-receive-server-first-message
           "bad iteration count\n  got: ~e" iters0))
  (define iters (string->number iters0))
  (define salt (base64-string-decode (hash-ref records #\s)))
  (define client-nonce (hash-ref ctx 'client-nonce))
  (define nonce (hash-ref records #\r))
  (unless (and (> (string-length nonce) (string-length client-nonce))
               (equal? client-nonce (substring nonce 0 (string-length client-nonce))))
    (error 'scram-receive-server-first-message
           "server nonce does not extend client nonce\n  server nonce: ~e\n  client nonce: ~e"
           nonce client-nonce))

  (define digest (hash-ref ctx 'digest))
  (define gs2-header (hash-ref ctx 'gs2-header))
  (define p-password (hash-ref ctx 'p-password))
  (define cbind (hash-ref ctx 'cbind))
  ;; SaltedPassword  := Hi(Normalize(password), salt, i)
  (define salted-password (pbkdf2 digest p-password salt iters))
  ;; ClientKey       := HMAC(SaltedPassword, "Client Key")
  (define client-key (hmac digest salted-password #"Client Key"))
  ;; ServerKey       := HMAC(SaltedPassword, "Server Key")
  (define server-key (hmac digest salted-password #"Server Key"))

  ;; cbind-input = gs2-header [ cbind-data ] -- present iff gs2-cbind-flag="p"
  (define cbind-input (string-append gs2-header (if (string? cbind) '??? "")))
  ;; channel-binding = "c=" <base64 encoding of cbind-input>
  ;; client-final-message-wo-proof = channel-binding "," nonce ["," extensions]
  (define msg-c2/no-proof (format "c=~a,r=~a" (string->base64 cbind-input) nonce))

  ;; StoredKey       := H(ClientKey)
  (define stored-key (md digest client-key))
  ;; AuthMessage     := client-first-message-bare + "," +
  ;;                    server-first-message + "," +
  ;;                    client-final-message-without-proof
  (define msg-c1/bare (hash-ref ctx 'msg-c1/bare))
  (define auth-message (string-append msg-c1/bare "," msg-s1 "," msg-c2/no-proof))
  ;; ClientSignature := HMAC(StoredKey, AuthMessage)
  (define client-signature (hmac digest stored-key auth-message))
  ;; ClientProof     := ClientKey XOR ClientSignature
  (define client-proof (bytes-xor client-key client-signature))
  ;; ServerSignature := HMAC(ServerKey, AuthMessage)
  (define server-signature (hmac digest server-key auth-message))
  ;; client-final-message = client-final-message-wo-proof "," proof
  (define msg-c2 (format "~a,p=~a" msg-c2/no-proof (base64-encode client-proof "")))

  (hash-set* ctx 'msg-s1 msg-s1 'salt salt 'iters iters 'nonce nonce
             'salted-password salted-password
             'client-key client-key 'server-key server-key
             'cbind-input cbind-input 'msg-c2/no-proof msg-c2/no-proof
             'stored-key stored-key 'auth-message auth-message
             'client-signature client-signature 'client-proof client-proof
             'server-signature server-signature 'msg-c2 msg-c2))

;; 3: C->S

(define (scram-client-final-message ctx)
  (hash-ref ctx 'msg-c2))

;; 4: S->C

(define (scram-receive-server-final-message ctx msg-s2)
  (define records (split-message msg-s2))
  (cond [(hash-ref records #\v)
         => (lambda (enc-verifier)
              (define server-signature (base64-decode (string->bytes/utf-8 enc-verifier)))
              (unless (equal? server-signature (hash-ref ctx 'server-signature))
                (error 'scram-receive-server-final-message
                       "invalid server signature"))
              (hash-set ctx 'msg-s2 msg-s2))]
        [(hash-ref records #\e)
         => (lambda (server-error)
              (error 'scram-receive-server-final-message
                     "error returned by server\n  error: ~e" server-error))]
        [else (error 'scram-receive-server-final-message
                     "invalid message: neither verifier nor error\n  message: ~e" msg-s2)]))

;; ------------------------------------------------------------

(define (encode-name s)
  (if (regexp-match? #rx"[=,]" s)
      (let ([out (open-output-string)])
        (for ([c (in-string s)])
          (case c
            [(#\,) (write-string "=2C" out)]
            [(#\=) (write-string "=3D" out)]
            [else (write-char c out)]))
        (get-output-string out))
      s))

(define (split-message msg)
  (define attrs
    (map decode-attr-val (string-split msg "," #:trim? #f)))
  (when (check-duplicates (map car attrs))
    (error 'split-message "duplicate attribute"))
  (make-hash attrs))

(define (decode-attr-val s)
  (define (bad) (error 'decode-attr-val "error parsing attr-val\n  input: ~e" s))
  (unless (>= (string-length s) 2) (bad))
  (unless (alpha-char? (string-ref s 0)) (bad))
  (unless (equal? (substring s 1 2) "=") (bad))
  (cons (string-ref s 0) (substring s 2)))

(define (alpha-char? c)
  (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))

(define (string->base64 s) (base64-encode (string->bytes/utf-8 s) ""))
(define (base64-string-decode s) (base64-decode (string->bytes/utf-8 s)))
