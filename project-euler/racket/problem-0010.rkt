#lang racket
#| http://projecteuler.net/problem=10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#
(define (problem-0010-a [limit 2000000])
  (for/sum ([p (in-producer (primes) (lambda (n) (>= n limit)))])
    p))

; use the Sieve of Eratosthenes
; source: http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(define (problem-0010-b [limit 2000000])
  (define v (make-vector limit #t))
  (for/sum ([i (in-range 2 limit)]
            #:when (vector-ref v i))
    (for ([j (in-range (* i i) limit i)])
      (vector-set! v j #f))
    i))

(define problem-0010 problem-0010-b)

; a generator for primes
(define (primes [n 2])
  (lambda ()
    (let loop ()
      (cond
        [(prime? n) 
         (set! n (+ n 1))
         (- n 1)]
        [else
         (set! n (+ n 1))
         (loop)]))))

; test if n is prime by trial division
(define (prime? n)
  (or (= n 2)
      (and (not (divides? n 2))
           (for/and ([i (in-range 3 (+ 1 (ceiling (sqrt n))) 2)])
             (not (divides? n i))))))

; test if n divides m
(define (divides? m n)
  (= 0 (remainder m n)))