#lang racket

#| http://projecteuler.net/problem=3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
|#
(define (problem-0003) (largest-prime-factor 600851475143))

; find the largest prime factor of a given number
(define (largest-prime-factor n)
  (apply max (prime-factors n)))

; find the prime factors of n
(define (prime-factors n)
  (define sqrtn (+ 1 (integer-sqrt n)))
  (let loop ([i 2])
    (cond
      [(> i sqrtn) (list n)]
      [(divides? n i) (cons i (prime-factors (/ n i)))]
      [else (loop (+ i 1))])))

; test if n evenly divides m
(define (divides? m n)
  (= 0 (remainder m n)))
