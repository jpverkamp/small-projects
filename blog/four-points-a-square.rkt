#lang racket

; standard distance function
(define (dist p1 p2)
  (define (sqr x) (* x x))
  (sqrt (+ (sqr (- (cadr p2) (cadr p1)))
           (sqr (- (car p2) (car p1))))))

; test if four points for a square
(define (square? pts)
  (define dists
    (sort 
     (list (dist (car pts) (cadr pts))
           (dist (car pts) (caddr pts))
           (dist (car pts) (cadddr pts)))
     <))
  (and (= (car dists)
          (cadr dists))
       (= (caddr dists)
          (* (sqrt 2) (car dists)))))
