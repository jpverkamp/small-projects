#lang racket

; split a number into its digits
(define (digits n)
  (let loop ([n n] [ls '()])
    (cond
      [(< n 10)
       (cons n ls)]
      [else
       (loop (quotient n 10)
             (cons (remainder n 10) ls))])))

; get only unique numbers from a list
(define (unique ls)
  (for/fold ([res '()])
            ([x (in-list ls)])
    (if (member x res)
        res
        (cons x res))))

; test if a number is pandigital
; must contain each digit exactly once
(define (pandigital? n)
  (define ls (sort (digits n) <))
  (define uls (sort (unique ls) <))
  (and (= (length uls) 10)
       (equal? ls uls)))

; combine a, b, and c into a number aaabbbcccc
(define (combine a b c)
  (+ (* (+ (* a 1000)
           b)
        10000)
     c))

; find all pandigital triples (a, b, a+b)
(define (pandigital-sums)
  (for*/list ([a (in-range 100 1000)]
              [b (in-range a 1000)]
              #:when (and (< 1000 (+ a b) 10000)
                          (pandigital? (combine a b (+ a b)))))
    (list a b (+ a b))))

; find the smallest number in all pandigital triples (a, b, a+b)
(define (smallest-in-pandigital-sums)
  (car (sort (apply append (pandigital-sums)) <)))