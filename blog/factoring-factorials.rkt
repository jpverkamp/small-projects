#lang racket

(require racket/splicing)

; Combine two hashes with numeric values by adding the values
(define (merge-hashes h1 h2)
  (define h (hash-copy h1))
  (for ([(k v) (in-hash h2)])
    (hash-update! h k (curry + v) 0))
  h)

; Memoized version of prime factorization
(splicing-let ([cache (make-hash)])
  (hash-set! cache 1 (hash 1 1))
  (hash-set! cache 2 (hash 2 1))
  (define (prime-factors n)
    (hash-ref! 
     cache n
     (thunk
       (or
        (for/first ([i (in-range 2 (add1 (sqrt n)))] #:when (zero? (remainder n i)))
          (merge-hashes (prime-factors i) (prime-factors (/ n i))))
        (hash n 1))))))

; actor n! without actually calculating the factorial first
(define (factor-factorial n)
  (for/fold ([factors (hash)]) ([i (in-range 2 (+ n 1))])
    (merge-hashes factors (prime-factors i))))

; Print out a factorial list (hash of primes to their power)
(define (print-factor-list fls)
  (printf 
   (string-join
    (for/list ([k (in-list (sort (hash-keys fls) <))])
      (format "~a^~a" k (hash-ref fls k)))
    " + "))
  (newline))
