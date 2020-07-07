#lang racket/base
(require rackunit
         racket/match
         racket/string
         racket/system
         net/base64
         sasl
         sasl/scram)

;; Test SCRAM client authentication against gsasl binary (if available).

;; This is a bit fragile, since it depends on the prompt strings that gsasl
;; produces (see `update-state`).

(define LOUD? #f)

;; gsasl-server : String (U Bytes #f) String -> (String -> Result)
;; Result = '(done) | (list 'running String)
(define (gsasl-server mechanism cb-data password)
  (match-define (list stdout stdin pid stderr ctl)
    (process* "/usr/bin/gsasl" "-s"
              "-m" (format "~a" mechanism)
              "-p" password))
  (define (read-until-wait p)
    (define acc (open-output-string))
    (let loop ()
      (when (sync/timeout 0.25 p)
        (define next (read-char p))
        (unless (eof-object? next)
          (write-char next acc)
          (loop))))
    (get-output-string acc))
  (define (get-out/err)
    (values (read-until-wait stdout) (read-until-wait stderr)))
  (define (decode s)
    (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 s))))
  (define (send-data data0)
    (define data (if (bytes? data0) data0 (string->bytes/utf-8 data0)))
    (fprintf stdin "~a\n" (base64-encode data ""))
    (flush-output stdin))
  (define (split-lines s)
    (let ([s (string-trim s #rx"\n" #:left? #f #:right? #t #:repeat? #f)])
      (string-split s #rx"\n" #:trim? #f)))

  (define state 'running) ;; 'running | 'done | 'error

  (define (update-state)
    (define-values (out prompt) (get-out/err))
    (cond [(regexp-match? #rx"Enter base64 encoded tls-unique channel binding:" out)
           (send-data (or cb-data ""))
           (update-state)]
          [else
           (define prompts (split-lines prompt))
           (define outs (split-lines out))
           (let loop ([prompts prompts] [outs outs])
             (match* [prompts outs]
               [['() '()] null]
               [[(cons (regexp #rx"Using mechanism:") prompts)
                 (cons mech outs)]
                (unless (equal? mech mechanism)
                  (error 'gsasl-server "mechanism mismatch: ~e vs ~e" mech mechanism))
                (cons mech (loop prompts outs))]
               [[(cons (regexp #rx"Enter base64 authentication data from client") prompts)
                 outs]
                (set! state 'running)
                (loop prompts outs)]
               [[(cons (regexp #rx"Output from server:") prompts)
                 (cons out outs)]
                (cons (decode out) (loop prompts outs))]
               [[(cons (regexp #rx"Server authentication finished \\(client trusted\\)") prompts)
                 outs]
                (set! state 'done)
                (loop prompts outs)]
               [[(cons (regexp #rx"Enter application data \\(EOF to finish\\)") prompts)
                 outs]
                (loop prompts outs)]
               [[_ _]
                (set! state 'error)
                (error 'gsasl-server "error\n  prompts: ~e\n  outs: ~e" prompts outs)]))]))

  (update-state)

  (define (g data)
    (send-data data)
    (let ([vs (update-state)]) (cons state vs)))
  g)

(define (test-handshake cctx sctx)
  ;; assumes client goes first
  (define (loop)
    ;; client's turn to send, but hasn't sent yet
    (case (sasl-state cctx)
      [(send/receive send/done)
       (sloop (sasl-next-message cctx))]
      [(done)
       (sloop #f)]
      [(error)
       (error 'test-handshake "failed: cctx in error state")]))
  (define (sloop cs)
    (when LOUD? (when cs (printf "-> ~a\n" cs)))
    (match (sctx (or cs ""))
      [(list 'running sc)
       (when LOUD? (printf "<- ~a\n" sc))
       (sasl-receive-message cctx sc)
       (loop)]
      [(list 'done)
       (void)]))
  (loop))

(define (call/custodian proc)
  (define cust (make-custodian))
  (parameterize ((current-custodian cust)
                 (current-subprocess-custodian-mode 'kill))
    (dynamic-wind void proc (lambda () (custodian-shutdown-all cust)))))

;; ============================================================

(define CBDATA #"123456789012")
(define BAD-CBDATA #"AAAABBBBCCCC")

(define gsasl-exe "/usr/bin/gsasl")

(unless (file-exists? gsasl-exe)
  (printf "Skipping tests against gsasl.\n"))

(when (file-exists? gsasl-exe)

  (test-case "gsasl scram-sha-1"
    (call/custodian
     (lambda ()
       (test-handshake (make-scram-client-ctx 'sha1 "user" "pencil")
                       (gsasl-server "SCRAM-SHA-1" #f "pencil")))))

  (test-case "gsasl scram-sha-1-plus"
    (call/custodian
     (lambda ()
       (test-handshake (make-scram-client-ctx 'sha1 "user" "pencil"
                                              #:channel-binding `(tls-unique ,CBDATA))
                       (gsasl-server "SCRAM-SHA-1-PLUS" CBDATA "pencil")))))

  (test-case "gsasl scram-sha-1 bad password"
    (call/custodian
     (lambda ()
       (check-exn #rx"Error authenticating user"
                  (lambda ()
                    (test-handshake (make-scram-client-ctx 'sha1 "user" "ballpoint")
                                    (gsasl-server "SCRAM-SHA-1" #f "pencil")))))))

  (test-case "gsasl scram-sha-1-plus bad cb"
    (call/custodian
     (lambda ()
       (check-exn #rx"Error authenticating user"
                  (lambda ()
                    (test-handshake (make-scram-client-ctx 'sha1 "user" "pencil"
                                                           #:channel-binding `(tls-unique ,CBDATA))
                                    (gsasl-server "SCRAM-SHA-1-PLUS" BAD-CBDATA "pencil")))))))

  (void))
