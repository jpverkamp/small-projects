#lang racket

(require 2htdp/image
         images/flomap)

; Resize a flomap using nearest-neighbors to preserve sharp edges
(define (flomap-resize/nn img new-width [new-height new-width])
  (match-define (flomap _ components old-width old-height) img)
  
  (build-flomap*
   components new-width new-height
   (λ (new-x new-y)
     (flomap-ref* img 
                  (floor (* old-width (/ new-x new-width)))
                  (floor (* old-height (/ new-y new-height)))))))

; Add a margin to a flomap
(define (flomap-add-margin img margin)
  (match-define (flomap _ components old-width old-height) img)
  
  (build-flomap*
   components
   (+ (* 2 margin) old-width)
   (+ (* 2 margin) old-height)
   (λ (x y)
     (cond
       [(and (<= margin x (+ margin old-width -1))
             (<= margin y (+ margin old-height -1)))
        (flomap-ref* img (- x margin) (- y margin))]
       [else 
        (make-vector components 1.0)]))))

; Create a symmetric 5x5 image similar to a space invader with this bit pattern
; 0 5 10 5 0
; 1 6 11 6 1
; 2 7 12 7 2
; 3 8 13 8 3
; 4 9 14 9 4
(define (procedural-invader i)
  (define bits 
    (for/vector ([c (in-string (~a (number->string i 2) #:width 15 #:pad-string "0" #:align 'right))])
      (eq? c #\1)))
  
  (build-flomap*
   1 5 5
   (λ (x y)
     (define i (+ y (* 5 (if (< x 3) x (- 4 x)))))
     (if (vector-ref bits i) '#(1.0) '#(0.0)))))

; Create a procedural invader as above, but highlight the point specified by four highest bits
(define (procedural-invader/highlight i)
  (define highlight (bitwise-and i #b1111))
  (define img-w/o-highlight (procedural-invader (arithmetic-shift i -4)))
  
  (build-flomap*
   3 5 5
   (λ (x y)
     (define i (+ y (* 5 (if (< x 3) x (- 4 x)))))
     (if (= highlight i)
         '#(1.0 0.0 0.0)
         (make-vector 3 (flomap-ref img-w/o-highlight 0 x y))))))

; Generate a demo sheet of procedural invaders
(define (demo [rows 5] [cols rows] #:image-size [image-size 20] #:margins [margins 5] #:highlights? [highlights? #f])
  (define images
    (for/list ([row (in-range rows)])
      (for/list ([col (in-range cols)])
        (define r (if highlights? (random 524288) (random 32768)))
        (define img ((if highlights? procedural-invader/highlight procedural-invader) r))
        (flomap->bitmap (flomap-add-margin (flomap-resize/nn img image-size) margins)))))
  
  (cond 
    [(and (= rows 1) (= cols 1))
     (caar images)]
    [(= rows 1)
     (car (map (curry apply beside) images))]
    [(= cols 1)
     (apply above (map car images))]
    [else
     (apply above (map (curry apply beside) images))]))

; Create a fractal of increasingly smaller procesdural invaders
(define (fractal-invaders width height #:highlights? [highlights? #f])
  (struct placed-invader (invader top left size))
  
  ; Minimum size is 7 to allow for a minimum margin of 1
  (define minimum-size 7) 
  
  ; Maximum size is 1/10 the smaller of width and height, but always at least the minimum
  (define maximum-size (max minimum-size (quotient (min width height) 10)))
  
  ; Test if two invaders overlap
  (define (overlap? a b)
    (match-define (placed-invader _ at al as) a)
    (match-define (placed-invader _ bt bl bs) b)
    (define ab (+ at as)) (define ar (+ al as))
    (define bb (+ bt bs)) (define br (+ bl bs))
    (cond
      ; One is entirely above the other
      [(or (< ab bt) (< bb at)) #f]
      ; One is to the left of the other
      [(or (< ar bl) (< br al)) #f]
      ; Otherwise, they overlap
      [else #t]))
  
  ; Given a list of invaders, add another that doesn't overlap with any of the previous
  (define (add-invader ls)
    (let loop ([tries 0])
      (cond
        [(>= tries 100) #f]
        [else
         (define size 
           (let loop () 
             (define r (random (min width height)))
             (if (<= minimum-size r maximum-size)
                 r 
                 (loop))))
         
         (define top (random (- height size)))
         (define left (random (- width size)))
         
         (define r (if highlights? (random 524288) (random 32768)))
         (define img ((if highlights? procedural-invader/highlight procedural-invader) r))
         (define invader (flomap-add-margin (flomap-resize/nn img (- size 2)) 1))
         
         (define new-invader (placed-invader invader top left size))
           
         (cond
           [(ormap (λ (other) (overlap? new-invader other)) ls)
            (loop (+ tries 1))]
           [else
            (cons new-invader ls)])])))
  
  ; Generate a list of fractal invaders until we cannot fill the space any more
  (define all-invaders
    (let loop ([ls '()])
      (cond
        [(add-invader ls) => loop]
        [else ls])))
  
  ; Render the invaders into a single image
  (flomap->bitmap
   (build-flomap*
    (if highlights? 3 1) width height
    (λ (x y)
      (cond
        [(for/first ([pi (in-list all-invaders)]
                     #:when (let ()
                              (match-define (placed-invader invader top left size) pi)
                              (and (<= left x (+ left size -1))
                                   (<= top  y (+ top  size -1)))))
           pi) => (λ (pi)
                    (match-define (placed-invader invader top left size) pi)
                    (flomap-ref* invader (- x left) (- y top)))]
        [else
         (if highlights? '#(1.0 1.0 1.0) '#(1.0))])))))
