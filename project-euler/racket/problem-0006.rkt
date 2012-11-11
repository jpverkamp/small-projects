#lang racket

#| http://projecteuler.net/problem=6

The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers
and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural
numbers and the square of the sum.
|#
(define (problem-0006 [n 100])
  (define ls (range 1 (+ n 1)))
  (- (square-of-sums ls)
     (sum-of-squares ls)))
      
; square the sum of a list
(define (square-of-sums ls)
  (sqr (apply + ls)))

; sum the squares of a list
(define (sum-of-squares ls)
  (apply + (map sqr ls)))

; square a number
(define (sqr x) (* x x))