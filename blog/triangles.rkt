#lang racket

(define-struct point (x y) #:transparent)

(define EPSILON 1e-10)
(define RIGHT-ANGLE (/ pi 2))

; clamp a number into a given range
(define (clamp n bound)
  (cond
    [(< n 0) (clamp (+ n bound) bound)]
    [(> n bound) (clamp (- n bound) bound)]
    [else n]))

; almost equal
(define (=ish . pts)
  (for*/and ([a (in-list pts)]
             [b (in-list pts)])
    (< (abs (- a b)) EPSILON)))

; almost greater than
(define (>ish . pts)
  (or (null? pts)
      (and (for*/and ([b (in-list (cdr pts))])
             (> (car pts) (+ b EPSILON)))
           (apply >ish (cdr pts)))))

; square a number
(define (sqr x)
  (* x x))

; distance between two points
(define (dist p1 p2)
  (sqrt (+ (sqr (- (point-x p1) (point-x p2)))
           (sqr (- (point-y p1) (point-y p2))))))

; calculate the angle at B from three points A, B, C
;     B
; ab / \ bc
;   A---C
;     ac
(define (angle a b c)
  (define ab (dist a b))
  (define bc (dist b c))
  (define ac (dist a c))
  (cond
    [(= (* 2 bc ac) 0)
     0]
    [else
     (clamp (acos (/ (+ (sqr bc) (sqr ac) (- (sqr ab)))
                     (* 2 bc ac)))
            pi)]))

; classify a triangle
; return equilateral/isosceles/scalene and acute/obtuse/right
;     B
; ab / \ bc
;   A---C
;     ac
(define (classify a b c)
  ; get sides
  (define ab (dist a b))
  (define bc (dist b c))
  (define ac (dist a c))
  (define dists (sort (list ab bc ac) >))
  
  ; get angles
  (define /_a (angle c a b))
  (define /_b (angle a b c))
  (define /_c (angle b c a))
  (define angles (sort (list /_a /_b /_c) >))

  ; sanity check
  (cond
    ; not a triangle
    [(or 
      ; angles must add up
      (not (=ish (apply + angles) pi))
      ; triangle inequality
      (or (>ish (car dists) (+ (cadr dists) (caddr dists)))
          (=ish (car dists) (+ (cadr dists) (caddr dists))))
      ; zero angles
      (or (=ish /_a 0)
          (=ish /_b 0)
          (=ish /_c 0)))
     #f]
    
    ; actually classify
    [else
     (list 
      ; equilateral/isosceles/scalene
      (cond
        [(=ish ab bc ac)
         'equilateral]
        [(or (=ish ab bc)
             (=ish ab ac)
             (=ish bc ac))
         'isosceles]
        [else
         'scalene])
      ; acute/obtuse/right
      (cond
        [(>ish (car angles) RIGHT-ANGLE)
         'obtuse]
        [(=ish (car angles) RIGHT-ANGLE)
         'right]
        [else
         'acute]))]))

(require rackunit)

; whee testing!
; source: https://code.google.com/codejam/contest/32014/dashboard
(for ([pts (in-list '((0 0 0 4 1 2)
                      (1 1 1 4 3 2)
                      (2 2 2 4 4 3)
                      (3 3 3 4 5 3)
                      (4 4 4 5 5 6)
                      (5 5 5 6 6 5)
                      (6 6 6 7 6 8)
                      (7 7 7 7 7 7)))]
      [ans (in-list '((isosceles obtuse)
                      (scalene acute)
                      (isosceles acute)
                      (scalene right)
                      (scalene obtuse)
                      (isosceles right)
                      #f
                      #f))])
  (check-equal?
   (classify (make-point (list-ref pts 0) (list-ref pts 1))
             (make-point (list-ref pts 2) (list-ref pts 3))
             (make-point (list-ref pts 4) (list-ref pts 5)))
   ans))
                             
