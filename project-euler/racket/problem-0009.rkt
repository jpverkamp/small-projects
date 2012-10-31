#lang racket

#| http://projecteuler.net/problem=9

A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.

Find the product abc.
|#
(define (problem-0009 [target 1000])
  (call/cc
   (lambda (return)
     (for* ([b (in-naturals 1)]
            [a (in-range 1 (+ b 1))])
       (define c (sqrt (+ (* a a) (* b b))))
       (when (and (integer? c)
                  (= target (+ a b c)))
         (return (* a b c)))))))
