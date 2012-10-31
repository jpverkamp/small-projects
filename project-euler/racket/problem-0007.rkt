#lang racket

#| http://projecteuler.net/problem=7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10 001st prime number?
|#
(define (problem-0007 [n 10001])
  (define primes... (primes))
  (for ([i (in-range (- n 1))]
        [p (in-producer primes... #f)])
    (void))
  (primes...))

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
  