#lang racket

#| http://projecteuler.net/problem=4

A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers.
|#
(define problem-0004 fastest-largest-palindrom-product)

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

; find the largest product of n digit numbers that's a palindrome
(define (largest-palindrome-product digits)
  (define lo (expt 10 (- digits 1)))
  (define hi (expt 10 digits))
  (for*/fold ([best 0])
             ([x (in-range lo hi)]
              [y (in-range x hi)]
              #:when (palindrome? (* x y)))
    (max best (* x y))))

; the palindrome came be written as abccba, which is
; 100000a + 10000b + 1000c + 100c + 10b + a
; = 100001a + 10010b + 1100c
; = 11(9091a + 910b + 100c)
; ergo one of the numbers is divisible by 11
(define (faster-largest-palindrome-product)
  (for*/fold ([best 0])
             ([x (in-range 11 1000 11)]
              [y (in-range x 1000)]
              #:when (palindrome? (* x y)))
    (max best (* x y))))
         
; the product can be written as (100a + 10b + c)(100d + 10e + f), which is
;                    100cd + 10ce + cf +
;           1000bd + 100be + 10bf +
; 10000ad + 1000ae + 100af
; assuming the first digit is 9, that means the last also is and as such cf = 9
; this means that cf are 1,9 / 3,3 / 7,7
; they are all odd, so both numbers will be odd
(define (fastest-largest-palindrome-product)
  (for*/fold ([best 0])
             ([x (in-range 11 1000 22)]
              [y (in-range x 1000 2)]
              #:when (palindrome? (* x y)))
    (max best (* x y))))

; this could probably be optimized even more using only numbers ending in 1/9, 3/3, 7/7
  