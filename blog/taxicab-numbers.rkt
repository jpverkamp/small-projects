#lang racket

(define (cube-pairs2 n)
  (define nthird (expt n 1/3))
  (let loop ([i 1])
    (cond
      [(> i nthird) '()]
      [else
       (define j (inexact->exact (round (expt (- n (* i i i)) 1/3))))
       (cond
         [(and (< i j)
               (= (+ (* i i i) (* j j j)) n))
          (cons (list i j) (loop (+ i 1)))]
         [else 
          (loop (+ i 1))])])))

; list all pairs a, b such that a^3 + b^3 = n
(define (cube-pairs n)  
  (for/list ([i (in-range 1 n)]
             #:when (integer? (expt (- n (expt i 3)) 1/3)))
    (list i (expt (- n (expt i 3)) 1/3))))

; list all pairs a, b such that a^3 + b^3 = n
(define (cube-pairs-2 n)  
  (define (j i) (inexact->exact (round (expt (- n (expt i 3)) 1/3))))
  (for/list ([i (in-range 1 (expt n 1/3))]
             #:when (let ([j (j i)])
                      (and (< i j)
                           (= (+ (* i i i) (* j j j)) n))))
    (list i (j i))))

; list all pairs a, b such that a^3 + b^3 = n
(define (cube-pairs-3 n)
  (define nthird (expt n 1/3))
  (let loop ([i 1])
    (cond
      [(> i nthird) '()]
      [else
       (define j (inexact->exact (round (expt (- n (* i i i)) 1/3))))
       (cond
         [(and (< i j)
               (= (+ (* i i i) (* j j j)) n))
          (cons (list i j) (loop (+ i 1)))]
         [else 
          (loop (+ i 1))])])))

; find the taxicab number for n
; this is the number such that their exists a1...an and b1...bn such that
; a1^3 + b1^3 = ... = an^3 + bn^3 = some number
; for example, Ta(2) = 1729 = 1^3 + 12^3 = 9^3 + 10^3
(define (taxicab n)
  (let loop ([i 1])
    (if (= n (length (cube-pairs-3 i)))
        (list i (cube-pairs-3 i))
        (loop (+ i 1)))))