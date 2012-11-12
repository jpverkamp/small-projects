#lang racket

#| http://projecteuler.net/problem=5

2520 is the smallest number that can be divided by each of the numbers from
1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
|#
(define (problem-0005) (least-common-multiple-up-to 20))
                        
; --- using my own fuctions ---

; calculate the least common multiple of a range
(define (least-common-multiple-up-to n)
  (for/fold ([res 1])
            ([i (in-range 1 (+ n 1))])
    (lcm^ res i)))

; find the least common multiple of two numbers
; source: http://en.wikipedia.org/wiki/Least_common_multiple
(define (lcm^ a b)
  (/ (abs (* a b)) (gcd^ a b)))

; find the greatest common divisor of two numbers
; source: http://en.wikipedia.org/wiki/Greatest_common_divisor
(define (gcd^ a b)
  (if (= b 0)
      a
      (gcd^ b (remainder a b))))

; --- using builtins ---

; calculate the least common multiple of a range
(define (least-common-multiple-up-to-builtin n)
  (apply lcm (range 1 (+ n 1))))