#lang racket

(provide sopf kappa msopf mkappa)

; does m divide into n evenly?
(define (divides? n m)
  (= 0 (remainder n m)))

; return a list of the prime factors of n
(define (prime-factors n)
  (define rootn (sqrt n))
  (cond
    [(<= n 1) '()]
    [(divides? n 2)
     (cons 2 (prime-factors (/ n 2)))]
    [else
     (let loop ([i 3])
       (cond
          [(> i rootn) (list n)]
          [(divides? n i)
           (cons i (prime-factors (/ n i)))]
          [else
           (loop (+ i 2))]))]))

; return only the unique items in a list
(define (unique ls)
  (reverse
   (for/fold ([ls '()]) ([x ls])
     (if (member x ls)
         ls
         (cons x ls)))))

; calculate the sum of unique prime factors of a number
(define (sopf n)
  (apply + (unique (prime-factors n))))

; determine the number of prime partitions of a number directly
(define (kappa n)
  (* (/ 1 n)
     (+ (sopf  n)
        (for/sum ([j (in-range 1 n)])
          (* (sopf j) (kappa (- n j)))))))

(require "memoize.rkt")

; calculate the sum of unique prime factors of a number
(define-memoized (msopf n)
  (apply + (unique (prime-factors n))))

; determine the number of prime partitions of a number directly
(define-memoized (mkappa n)
  (* (/ 1 n)
     (+ (msopf n)
        (for/sum ([j (in-range 1 n)])
          (* (msopf j) (mkappa (- n j)))))))

