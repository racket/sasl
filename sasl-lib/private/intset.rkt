#lang racket/base
(require (for-syntax racket/base syntax/parse racket/match))
(provide intset
         char-predicate
         int-in-set?
         char-in-set?)

(define-syntax (intset stx)
  (define-syntax-class range
    (pattern [lo:nat hi:nat])
    (pattern lo:nat #:with hi #'lo))
  (syntax-parse stx
    [(_ r:range ...)
     (define ranges0 (syntax->datum #'((r.lo r.hi) ...)))
     (define (compress ranges)
       (match ranges
         [(list* (list lo1 hi1) (list lo2 hi2) more)
          (cond [(= (add1 hi1) lo2)
                 (compress (list* (list lo1 hi2) more))]
                [else
                 (cons (car ranges) (compress (cdr ranges)))])]
         [else ranges]))
     #`(quote #,(list->vector (apply append (compress ranges0))))]))

(define ((char-predicate . sets) c)
  (for/or ([s (in-list sets)]) (char-in-set? c s)))

(define (char-in-set? c is) (int-in-set? (char->integer c) is))

;; An IntSet is (Vector {lo hi} ...)
;; Intepretation: (lambda (x) (or (<= lo x hi) ...))

(define (int-in-set?/linear n is)
  (for/or ([i (in-range 0 (vector-length is) 2)])
    (<= (vector-ref is i) n (vector-ref is (add1 i)))))

(define (int-in-set? seek is)
  ;; (eprintf "seeking ~s\n" seek)
  (define (value k) (vector-ref is k))
  (define (loop lo hi)
    ;; (eprintf "  loop ~s, ~s\n" lo hi)
    (and (< lo hi) (loop* lo hi)))
  (define (loop* lo hi)
    ;; INV: (<= (value lo) seek (value hi))
    ;; INV: (even? lo) and (odd? hi)
    (define midlo (* 2 (quotient (+ lo hi 1) 4)))
    (define midhi (add1 midlo))
    (cond [(< seek (value midlo))
           (loop lo (sub1 midlo))]
          [(< (value midhi) seek)
           (loop (add1 midhi) hi)]
          ;; (value midlo) <= seek <= (value midhi)
          [else #t]))
  (let ([last (sub1 (vector-length is))])
    (cond [(<= (value 0) seek (value last))
           (loop 0 last)]
          [else #f])))

(module+ test
  (define v #(2 4 7 7 9 9 15 19 21 21))
  (for ([i (in-range 25)])
    (define r1 (int-in-set?/linear i v))
    (define r2 (int-in-set? i v))
    (unless (equal? r1 r2)
      (error "disagree on ~s: ~s vs ~s" i r1 r2))))
