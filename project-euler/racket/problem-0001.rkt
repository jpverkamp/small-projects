#lang racket

#| http://projecteuler.net/problem=1

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#
(define (problem-0001) (sum-divisibles 1000 '(3 5)))
  
; sum all numbers divisible by numbers in a given list up to a given limit
(define (sum-divisibles limit nls)
  (for/sum ([i (in-range 1 limit)]
            #:when (ormap (lambda (j) (divides? i j)) nls))
    i))

; test if n evenly divides m
(define (divides? m n)
  (= 0 (remainder m n)))