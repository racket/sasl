#lang racket/base

(require rackunit
         sasl)

(test-case "noop context"
  (define ctx (make-sasl-ctx #f #f 'done))
  (check-equal? (sasl-state ctx) 'done)
  (check-exn #rx"sequence violation" (λ () (sasl-next-message ctx)))
  (check-exn #rx"sequence violation" (λ () (sasl-receive-message ctx "fail"))))

(test-case "custom context without receive"
  (define ctx (make-sasl-ctx #f "data" 'done))
  (check-equal? (sasl-state ctx) 'send/done)
  (check-equal? (sasl-next-message ctx) "data")
  (check-exn #rx"sequence violation" (λ () (sasl-receive-message ctx "fail"))))

(test-case "custom context with immediate receive state"
  (define ctx (make-sasl-ctx #f #f (λ (_ data) (values data 'done))))
  (check-equal? (sasl-state ctx) 'receive)
  (check-exn #rx"sequence violation" (λ () (sasl-next-message ctx)))
  (sasl-receive-message ctx "hello")
  (check-equal? (sasl-state ctx) 'send/done)
  (check-equal? (sasl-next-message ctx) "hello")
  (check-exn #rx"sequence violation" (λ () (sasl-receive-message ctx "fail"))))

(define (salt s data)
  (sha1-bytes (bytes-append s data)))

(define (make-example-context username password shared-secret)
  (define ht
    (hasheq 'username username
            'password password
            'shared shared-secret))
  (make-sasl-ctx ht username example-receive-1))

(define (example-receive-1 h data)
  (values (salt data (hash-ref h 'password)) example-receive-2))

(define (example-receive-2 h data)
  (define expected
    (salt (hash-ref h 'shared)
          (hash-ref h 'password)))
  (unless (equal? expected data)
    (error "shared check failed"))
  (values #f 'done))

(test-case "custom context happy path"
  (define password #"hunter2")
  (define secret #"super-secret")
  (define ctx (make-example-context #"user" password secret))
  (check-equal? (sasl-state ctx) 'send/receive)
  (check-equal? (sasl-next-message ctx) #"user")
  (sasl-receive-message ctx #"a-salt")
  (check-equal? (sasl-state ctx) 'send/receive)
  (check-equal? (sasl-next-message ctx) (salt #"a-salt" password))
  (sasl-receive-message ctx (salt secret password))
  (check-equal? (sasl-state ctx) 'done))

(test-case "custom context contract error during receive"
  (define ctx (make-example-context #"user" #"hunter2" #"secret"))
  (check-exn
   #rx"contract violation"
   (λ () (sasl-receive-message ctx #f)))
  (check-equal? (sasl-state ctx) 'send/receive))

(test-case "custom context failure during receive"
  (define ctx (make-example-context #"user" #"hunter2" #"secret"))
  (sasl-receive-message ctx #"a-salt")
  (check-exn
   #rx"custom context error: shared check failed"
   (λ () (sasl-receive-message ctx #"unexpected")))
  (check-equal? (sasl-state ctx) 'error))
