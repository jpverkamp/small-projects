#lang racket

#| http://projecteuler.net/problem=7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10,001st prime number?
|#
(define (problem-0007)
  (nth-prime 10001))

; return the nth prime
(define (nth-prime n)
  (let loop ([n n] [p 1])
    (if (= n 0)
        p
        (loop (- n 1) (next-prime p)))))

; return the next prime after a given number
(define (next-prime n)
  (let loop ([n (+ n 1)])
    (if (prime? n)
        n
        (loop (+ n 1)))))

; test if n is prime by trial division
(define (prime? n)
  (or (= n 2)
      (and (not (divides? n 2))
           (for/and ([i (in-range 3 (+ 1 (ceiling (sqrt n))) 2)])
             (not (divides? n i))))))

; test if n divides m
(define (divides? m n)
  (= 0 (remainder m n)))