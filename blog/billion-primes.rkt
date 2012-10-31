#lang racket

(provide sum-primes-direct
         sum-primes-list
         sum-primes-sieve-eratosthenes-list
         sum-primes-sieve-eratosthenes-vector
         sum-primes-sieve-eratosthenes-multivector
         sum-primes-sieve-eratosthenes-bitvector
         sum-primes-sieve-atkin
         sum-primes-sieve-sundaram)

; test if n divides m
(define (divides? m n)
  (= 0 (remainder m n)))

; test if n is prime by trial division
(define (prime? n)
  (and (not (divides? n 2))
       (for/and ([i (in-range 3 (+ 1 (ceiling (sqrt n))) 2)])
         (not (divides? n i)))))

; estimate the nth prime, return lower and upper bounds
; source: http://en.wikipedia.org/wiki/Prime_number_theorem
(define (guess-nth-prime n)
  (values (inexact->exact 
           (floor (* n (log n))))
          (inexact->exact 
           (ceiling
            (if (<= n 1) 
                3 
                (+ (* n (log n)) (* n (log (log n)))))))))

; sum the first n primes directly
(define (sum-primes-direct n)
  (let loop ([i 3] [count 1] [sum 2])
    (cond
      [(= count n) sum]
      [(prime? i)
       (loop (+ i 2) (+ count 1) (+ sum i))]
      [else
       (loop (+ i 2) count sum)])))

; sum the first n primes by keeping a list of primes to divide by
(define (sum-primes-list n)
  (let loop ([i 3] [count 1] [sum 2] [primes '()])
    (cond
      [(= count n)
       sum]
      [(andmap (lambda (prime) (not (divides? i prime))) primes)
       (loop (+ i 2) (+ count 1) (+ sum i) (cons i primes))]
      [else
       (loop (+ i 2) count sum primes)])))

; sum the first n primes using the Sieve of Eratosthenes
; algorithm source: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(define (sum-primes-sieve-eratosthenes-list n)
  (define-values (lo hi) (guess-nth-prime n))
  (let loop ([ls (range 2 hi)]
             [count 0]
             [sum 0])
    (cond
      [(= count n) sum]
      [else
       (loop 
        (filter
         (lambda (i) (not (divides? i (car ls))))
         (cdr ls))
        (+ count 1)
        (+ (car ls) sum))])))

; sum the first n primes using the Sieve of Eratosthenes with a vector
; algorithm source: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(define (sum-primes-sieve-eratosthenes-vector n)
  (define-values (lo hi) (guess-nth-prime n))
  (define v (make-vector hi #t))
  (vector-set! v 0 #f)
  (vector-set! v 1 #f)
  (for* ([i (in-range 2 hi)]
         #:when (vector-ref v i)
         [j (in-range (* i i) hi i)])
    (vector-set! v j #f))
  (let loop ([i 3] [count 1] [sum 2])
    (cond
      [(= count n) sum]
      [(vector-ref v i)
       (loop (+ i 2) (+ count 1) (+ sum i))]
      [else
       (loop (+ i 2) count sum)])))

; sum the first n primes using the Sieve of Eratosthenes with a bitvector
; algorithm source: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(require "bitvector.rkt")
(define (sum-primes-sieve-eratosthenes-bitvector n)
  (define-values (lo hi) (guess-nth-prime n))
  (define v (make-bitvector hi #t))
  (bitvector-set! v 0 #f)
  (bitvector-set! v 1 #f)
  (for* ([i (in-range 2 hi)]
         #:when (bitvector-ref v i)
         [j (in-range (* i i) hi i)])
    (bitvector-set! v j #f))
  (let loop ([i 3] [count 1] [sum 2])
    (cond
      [(= count n) sum]
      [(bitvector-ref v i)
       (loop (+ i 2) (+ count 1) (+ sum i))]
      [else
       (loop (+ i 2) count sum)])))

; sum the first n primes using the Sieve of Eratosthenes with a multivector
; algorithm source: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(require "multivector.rkt")
(define (sum-primes-sieve-eratosthenes-multivector n)
  (define-values (lo hi) (guess-nth-prime n))
  (define v (make-multivector hi 100 #t))
  (multivector-set! v 0 #f)
  (multivector-set! v 1 #f)
  (for* ([i (in-range 2 hi)]
         #:when (multivector-ref v i)
         [j (in-range (* i i) hi i)])
    (multivector-set! v j #f))
  (let loop ([i 3] [count 1] [sum 2])
    (cond
      [(= count n) sum]
      [(multivector-ref v i)
       (loop (+ i 2) (+ count 1) (+ sum i))]
      [else
       (loop (+ i 2) count sum)])))

; sum the first n primes using the Sieve of Atkin
; algorithm source: http://en.wikipedia.org/wiki/Sieve_of_Atkin
(define (sum-primes-sieve-atkin n)
  (define-values (lo hi) (guess-nth-prime n))
  (define v (make-vector hi #f))
  ; add candidate primes
  (for* ([x (in-range 1 (+ 1 (sqrt hi)))]
         [y (in-range 1 (+ 1 (sqrt hi)))])
    (define x2 (* x x))
    (define y2 (* y y))
    (let ([i (+ (* 4 x2) y2)])
      (when (and (< i hi) (or (= 1 (remainder i 12))
                               (= 5 (remainder i 12))))
        (vector-set! v i (not (vector-ref v i)))))
    (let ([i (+ (* 3 x2) y2)])
      (when (and (< i hi) (= 7 (remainder i 12)))
        (vector-set! v i (not (vector-ref v i)))))
    (let ([i (- (* 3 x2) y2)])
      (when (and (> x y) (< i hi) (= 11 (remainder i 12)))
        (vector-set! v i (not (vector-ref v i))))))
  ; remove composites
  (for ([i (in-range 5 (+ 1 (sqrt hi)))])
    (when (vector-ref v i)
      (for ([k (in-range (* i i) hi (* i i))])
        (vector-set! v k #f))))
  ; report
  (let loop ([i 5] [count 2] [sum 5])
    (cond
      [(= count n) sum]
      [(vector-ref v i)
       (loop (+ i 2) (+ count 1) (+ sum i))]
      [else
       (loop (+ i 2) count sum)])))

; sum the first n primes using the Sieve of Sundaram
; algorithm source: http://en.wikipedia.org/wiki/Sieve_of_Sundaram
(define (sum-primes-sieve-sundaram n)
  (define-values (lo hi) (guess-nth-prime n))
  (define dn (quotient hi 2))
  (define v (make-vector dn #t))
  (for* ([j (in-range 1 dn)]
         [i (in-range 1 (+ j 1))]
         #:when (< (+ i j (* 2 i j)) dn))
    (vector-set! v (+ i j (* 2 i j)) #f))
  (let loop ([i 1] [count 1] [sum 2])
    (cond
      [(= count n) sum]
      [(vector-ref v i)
       (loop (+ i 1) (+ count 1) (+ sum (+ 1 (* 2 i))))]
      [else
       (loop (+ i 1) count sum)])))

           
