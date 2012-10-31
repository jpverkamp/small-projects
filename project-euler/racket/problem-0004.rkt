#lang racket

#| http://projecteuler.net/problem=4

A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers.
|#
(define (problem-0004 [digits 3])
  (define lo (expt 10 (- digits 1)))
  (define hi (expt 10 digits))
  (inexact->exact
   (for*/fold ([best -inf.0])
              ([x (in-range lo hi)]
               [y (in-range x hi)]
               #:when (palindrome? (* x y)))
     (max best (* x y)))))

; test if a number is a palindrome
(define (palindrome? n)
  (equal? (digits n)
          (reverse (digits n))))

; return the digits of n
(define (digits n)
  (let loop ([n n] [ls '()])
    (cond
      [(< n 10) (cons n ls)]
      [else (loop (quotient n 10)
                  (cons (remainder n 10) ls))])))
  