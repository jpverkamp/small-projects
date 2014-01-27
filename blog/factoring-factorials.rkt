#lang racket

(require racket/splicing)

; Combine two hashes with numeric values by adding the values
(define/contract (merge-hashes h1 h2)
  (define h (hash-copy h1))
  (for ([(k v) (in-hash h2)])
    (hash-update! h k (curry + v) v))
  h)

; Memoized version of prime factorization
(splicing-let ([cache (make-hash)])
  (define (prime-factors n)
    (hash-ref! 
     cache n
     (or
      (for/first ([i (in-range 2 (sqrt n))] #:when (zero? (remainder n i)))
        (merge-hashes (prime-factors i) (prime-factors (/ n i))))
      (hash n 1)))))

; Factor n! without actually calculating the factorial first
(define (factor-factorial n)
  (for/fold ([factors (hash)]) ([i (in-range 1 (+ n 1))])
    (merge-hashes factors (prime-factors i))))

; Print out a factorial list (hash of primes to their power)
(define (print-factor-list fls)
  (printf 
   (string-join
    (for/list ([k (in-list (sort (hash-keys fls) <))])
      (format "~a^~a" k (hash-ref fls k)))
    " + "))
  (newline))
