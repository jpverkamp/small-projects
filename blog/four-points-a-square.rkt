#lang racket

(require rackunit)

; square a number
(define (sqr x) 
  (* x x))

; standard distance function
(define (dist p1 p2)
  (sqrt (+ (sqr (- (cadr p2) (cadr p1)))
           (sqr (- (car p2) (car p1))))))

; test if four points form a square
(define (square? pts)
  ; calculate all pairwise distances
  (define dists
    (sort 
     (for*/list ([a (in-list pts)]
                 [b (in-list pts)])
       (dist a b))
     <))
  
  ; we should have:
  (and 
   ; 4x zero for self matches
   (apply = (cons 0 (take dists 4)))
   ; 8x identical distances for the sides
   (apply = (take (drop dists 4) 8))
  ; 4x sqrt(2) * side for the diagonals
   (apply = (cons (* (sqrt 2) (car (drop dists 4)))
                  (drop dists 12)))))


(check-true (square? '((0 0) (0 1) (1 0) (1 1))) "unit square")
(check-true (square? '((0 0) (2 1) (3 -1) (1 -2))) "at an angle")
(check-true (square? '((0 0) (1 1) (0 1) (1 0))) "re-ordered points")

(check-false (square? '((0 0) (0 2) (3 2) (3 0))) "rectangle")
(check-false (square? '((0 0) (3 4) (8 4) (5 0))) "rhombus")
(check-false (square? '((0 0) (0 0) (1 1) (0 0))) "not a polygon")
(check-false (square? '((0 0) (0 0) (1 0) (0 1))) "not a polygon")

(check-false (square? '((0 0) (0 1) (1 0) (-1 -1))) "fails simple one point test")