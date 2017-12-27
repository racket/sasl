#lang racket/base
(require rackunit
         net/base64
         sasl
         sasl/private/scram)

(test-case "scram-sha1"
  (define ctx (make-scram-client-ctx 'sha1 "user" "pencil"
                                     #:client-nonce "fyko+d2lbbFgONRv9qkxdawL"))
  (check-equal? (sasl-next-message ctx) "n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL")
  (sasl-receive-message ctx
    "r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096")
  (check-equal?
   (sasl-next-message ctx)
   "c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=")
  (sasl-receive-message ctx "v=rmF9pqV8S7suAoZWja4dJRkFsKQ=")
  (check-eq? (sasl-state ctx) 'done))

(test-case "scram-sha256"
  (define ctx (make-scram-client-ctx 'sha256 "user" "pencil"
                                     #:client-nonce "rOprNGfwEbeRWgbNEkqO"))
  (check-equal? (sasl-next-message ctx) "n,,n=user,r=rOprNGfwEbeRWgbNEkqO")
  (sasl-receive-message ctx
    "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096")
  (check-equal? (sasl-next-message ctx)
                (string-append "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,"
                               "p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ="))
  (sasl-receive-message ctx "v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=")
  (check-eq? (sasl-state ctx) 'done))

;; ----

(test-case "scram-sha256 bad nonce"
  (define ctx (make-scram-client-ctx 'sha256 "user" "pencil"
                                     #:client-nonce "rOprNGfwEbeRWgbNEkqO"))
  (check-equal? (sasl-next-message ctx) "n,,n=user,r=rOprNGfwEbeRWgbNEkqO")
  (check-exn #rx"sasl-receive-message: got bad nonce from server"
             (lambda ()
               (sasl-receive-message ctx "r=BADNONCE,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096")))
  (check-eq? (sasl-state ctx) 'error))

(test-case "scram-sha256 bad iters"
  (define ctx (make-scram-client-ctx 'sha256 "user" "pencil"
                                     #:client-nonce "rOprNGfwEbeRWgbNEkqO"))
  (check-equal? (sasl-next-message ctx) "n,,n=user,r=rOprNGfwEbeRWgbNEkqO")
  (check-exn #rx"sasl-receive-message: got bad iteration count"
             (lambda ()
               (sasl-receive-message
                ctx (string-append "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,"
                                   "s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096BADITERS"))))
  (check-eq? (sasl-state ctx) 'error))

(test-case "scram-sha256 bad signature"
  (define ctx (make-scram-client-ctx 'sha256 "user" "pencil"
                                     #:client-nonce "rOprNGfwEbeRWgbNEkqO"))
  (check-equal? (sasl-next-message ctx) "n,,n=user,r=rOprNGfwEbeRWgbNEkqO")
  (sasl-receive-message ctx
    "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096")
  (check-equal? (sasl-next-message ctx)
                (string-append "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,"
                               "p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ="))
  (check-exn #rx"sasl-receive-message: received invalid signature from server"
             (lambda () (sasl-receive-message ctx (format "v=~a" (base64-encode #"BADSIG")))))
  (check-eq? (sasl-state ctx) 'error))
