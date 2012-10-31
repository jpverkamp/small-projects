#lang racket

#| http://projecteuler.net/problem=1

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#
Find the sum of all the multiples of 3 or 5 below 1000.
(define (problem-0001 [limit 1000])
  (for/sum ([i (in-range 1 limit)]
            #:when (or (divides? i 3)
                       (divides? i 5)))
    i))

; test if n evenly divides m
(define (divides? m n)
  (= 0 (remainder m n)))