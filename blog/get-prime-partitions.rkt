#lang racket

(provide prime-partitions)

(require racket/generator)

; does m divide into n evenly?
(define (divides? n m)
  (= 0 (remainder n m)))

; is n prime?
(define (prime? n)
  (define rootn (sqrt n))
  (cond
    [(= n 1) #f]
    [(divides? n 2) #f]
    [else
     (let loop ([i 3])
       (cond
          [(> i rootn) #t]
          [(divides? n i) #f]
          [else
           (loop (+ i 2))]))]))

; create a generator of primes between min and max (inclusive)
; if max is #f, return an infinite generator
(define (primes [min 2] [max #f])
  (generator ()
    (when (>= 2 min)
      (yield 2))
    (let loop ([i 3])
      (when (or (not max) (<= i max))
        (when (and (>= i min) (prime? i))
          (yield i))
        (loop (+ i 2))))
    (yield #f)))

; calculate the prime partitions of n
; min is the smallest prime to consider for the recursion
(define (prime-partitions n [min 2])
  (cond
    [(< n 0) '()]
    [(= n 0) '(())]
    [else
     (for*/list ([i (in-producer (primes min n) #f)]
                 [r (prime-partitions (- n i) i)])
       (cons i r))]))