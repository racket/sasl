#lang racket/base
(require "private/intset.rkt"
         "private/stringprep.rkt")
(provide saslprep)

;; References:
;; - https://tools.ietf.org/html/rfc3454 (stringprep framework)
;; - https://tools.ietf.org/html/rfc4013 (SASLprep profile)

;; saslprep : String -> String
(define (saslprep orig #:allow-unassigned? [allow-unassigned? #f])
  (let* ([s (do-mapping orig)]
         [s (do-normalize s orig)])
    (do-prohibit s orig allow-unassigned?)
    (do-check-bidi s orig)
    s))

;; 1. Mapping

(define (do-mapping s)
  (define out (open-output-string))
  (for ([c (in-string s)])
    (cond [(map-to-space? c) (write-char #\space out)]
          [(map-to-nothing? c) (void)]
          [else (write-char c out)]))
  (get-output-string out))

(define map-to-space?   (char-predicate non-ascii-space-characters))
(define map-to-nothing? (char-predicate commonly-mapped-to-nothing))

;; 2. Normalize

(define (do-normalize s orig)
  (string-normalize-nfkc s))

;; 3. Prohibit

(define (do-prohibit s orig allow-unassigned?)
  (for ([c (in-string s)])
    (define (bad msg) (error 'saslprep "~a\n  string: ~e\n  prohibited: ~e" msg orig c))
    (when (prohibited-char? c)
      (bad "prohibited character in string"))
    (when (and (not allow-unassigned?) (unassigned-char? c))
      (bad "unassigned character in string"))))

(define (prohibited-char? c) (char-in-set? c prohibited-characters))
(define unassigned-char? (char-predicate unassigned-in-unicode-3.2))

;; 4. Check Bidi

(define (do-check-bidi s orig)
  (define (bad msg)
    (error 'saslprep "bidirectional check failed;\n ~a\n  string: ~e" msg orig))
  (cond [(zero? (string-length s)) (void)]
        [(for/or ([c (in-string s)]) (RandAL-char? c))
         (for ([c (in-string s)])
           (when (L-char? c)
             (bad "string contains both R/AL and L characters")))
         (unless (and (RandAL-char? (string-ref s 0))
                      (RandAL-char? (string-ref s (sub1 (string-length s)))))
           (bad "string containing R/AL characters must start and end with R/AL"))]
        [else (void)]))

(define RandAL-char? (char-predicate RandALCat-characters))
(define L-char?      (char-predicate LCat-characters))

;; ----------------------------------------
;; Character range tables

(define prohibited-characters
  '#(#x0000 #x001F
     #x007F #x00A0
     #x0340 #x0341
     #x06DD #x06DD
     #x070F #x070F
     #x1680 #x1680
     #x180E #x180E
     #x2000 #x200F
     #x2028 #x202F
     #x205F #x2063
     #x206A #x206F
     #x2FF0 #x2FFB
     #x3000 #x3000
     #xD800 #xF8FF
     #xFDD0 #xFDEF
     #xFEFF #xFEFF
     #xFFF9 #xFFFF
     #x1D173 #x1D17A
     #x1FFFE #x1FFFF
     #x2FFFE #x2FFFF
     #x3FFFE #x3FFFF
     #x4FFFE #x4FFFF
     #x5FFFE #x5FFFF
     #x6FFFE #x6FFFF
     #x7FFFE #x7FFFF
     #x8FFFE #x8FFFF
     #x9FFFE #x9FFFF
     #xAFFFE #xAFFFF
     #xBFFFE #xBFFFF
     #xCFFFE #xCFFFF
     #xDFFFE #xDFFFF
     #xE0001 #xE0001
     #xE0020 #xE007F
     #xEFFFE #xEFFFF
     #xF0000 #xFFFFF
     #x100000 #x10FFFF))
