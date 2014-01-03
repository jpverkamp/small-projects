#lang racket

(struct point     (x y)           #:transparent)
(struct circle    (center radius) #:transparent)

; Test if circle contains a given point
(define (contains? c p)
  (<= (+ (sqr (- (point-x (circle-center c)) (point-x p)))
         (sqr (- (point-y (circle-center c)) (point-y p))))
      (sqr (circle-radius c))))

; Check if any circle in a given list cs contains a point (x y)
(define (any-contains? cs p)
  (ormap (λ (c) (contains? c p)) cs))

; Return a given number chosen uniformly from [min, max)
(define (rand-range min max)
  (+ min (* (random) (- max min))))
     
; Calculate the area of a given circle
(define (area-of circles #:samples [samples 1e6])
  ; Find the range of the circles
  (define-values (x-min x-max y-min y-max)
    (for/fold ([x-min +inf.0] [x-max -inf.0]
               [y-min +inf.0] [y-max -inf.0])
              ([c (in-list circles)])
      (values (min x-min (- (point-x (circle-center c)) (circle-radius c)))
              (max x-max (+ (point-x (circle-center c)) (circle-radius c)))
              (min y-min (- (point-y (circle-center c)) (circle-radius c)))
              (max y-max (+ (point-y (circle-center c)) (circle-radius c))))))
  
  ; Count how many random darts hit any circle
  (define hits
    (for/sum ([i (in-range samples)])
      (define dart 
        (point (rand-range x-max x-min)
               (rand-range y-max y-min)))
      (if (any-contains? circles dart) 1 0)))
  
  ; Use that ratio to calculate the area of the circles
  (* (/ hits samples)
     (* (- x-max x-min)
        (- y-max y-min))))

(module+ test
  (require rackunit)
  
  (check-=
   (area-of (list (circle (point 0 0) 1)))
   pi
   0.1
   "Unit circle")
  
  (check-=
   (area-of (list (circle (point 0 0) 10)))
   (* pi 10 10)
   1
   "Big circle")
  
  (check-=
   (area-of (list (circle (point 0 0) 0.1)))
   (* pi 0.1 0.1)
   0.01
   "Small circle")
  
  (check-=
   (area-of (list (circle (point 3 4) 1)))
   pi
   0.1
   "Offset circle")
  
  (check-=
   (area-of (list (circle (point 0 0) 1)
                  (circle (point 0 0) 0.5)))
   pi
   0.1
   "Smaller circle completely enclosed")
  
  (check-=
   (area-of (list (circle (point -2 0) 1)
                  (circle (point  2 0) 1)))
   (* 2 pi)
   0.1
   "Two distinct circles")
  
  (check-=
   (area-of (list (circle (point -0.5 0) 1)
                  (circle (point  0.5 0) 1)))
   5.0548
   0.1
   "Overlapping circles"))
