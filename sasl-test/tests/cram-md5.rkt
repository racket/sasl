#lang racket/base
(require rackunit
         sasl
         sasl/private/cram-md5)

(check-equal? (cram-md5-client-response
               "joe" "tanstaaftanstaaf"
               "<1896.697170952@postoffice.example.net>")
              "joe 3dbc88f0624776a737b39093f6eb6427")

(check-equal? (cram-md5-client-response
               "Ali Baba" "Open, Sesame"
               "<68451038525716401353.0@localhost>")
              "Ali Baba 6fa32b6e768f073132588e3418e00f71")

(check-equal? (cram-md5-client-response
               "Al\u00AAdd\u00ADin\u00AE" "Open, Sesame"
               "<92230559549732219941.0@localhost>")
              (bytes->string/utf-8
               #"Aladdin\xC2\xAE 9950ea407844a71e2f0cd3284cbd912d"))

(test-case "cram-md5 joe"
  (define ctx (make-cram-md5-client-ctx "joe" "tanstaaftanstaaf"))
  (sasl-receive-message ctx "<1896.697170952@postoffice.example.net>")
  (check-equal? (sasl-next-message ctx)
                "joe 3dbc88f0624776a737b39093f6eb6427")
  (check-eq? (sasl-state ctx) 'send/done))

(test-case "cram-md5 ali baba"
  (define ctx (make-cram-md5-client-ctx "Ali Baba" "Open, Sesame"))
  (sasl-receive-message ctx "<68451038525716401353.0@localhost>")
  (check-equal? (sasl-next-message ctx)
                "Ali Baba 6fa32b6e768f073132588e3418e00f71")
  (check-eq? (sasl-state ctx) 'send/done))

(test-case "cram-md5 aladdin"
  (define ctx (make-cram-md5-client-ctx "Al\u00AAdd\u00ADin\u00AE" "Open, Sesame"))
  (sasl-receive-message ctx "<92230559549732219941.0@localhost>")
  (check-equal? (sasl-next-message ctx)
                (bytes->string/utf-8
                 #"Aladdin\xC2\xAE 9950ea407844a71e2f0cd3284cbd912d"))
  (check-eq? (sasl-state ctx) 'send/done))
