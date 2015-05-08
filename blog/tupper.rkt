#lang racket

(require images/flomap
         mrlib/gif)

; Modulus that will work with really large numbers
(define (mod a b)
  (define q (floor (/ a b)))
  (define r (- a (* b q)))
  r)

; Tupper's "self-referential" formula
; Encodes a bitmap as an integer
(define (tupper k)
  (flomap->bitmap
   (build-flomap*
    1 106 17
    (λ (x y)
      (set! y (+ y k))
      (set! x (- 105 x))
      (cond
        [(< 1/2 (floor (mod (* (floor (/ y 17)) (expt 2 (- (* -17 (floor x)) (mod (floor y) 17)))) 2)))
         (vector 0)]
        [else
         (vector 1)])))))

(define (render-to target)
  (define str-target (number->string target))
  (define str-buffer (make-string (string-length str-target) #\0))
  
  (for/list ([i (in-range (sub1 (string-length str-target)) -1)])
    (string-set! str-buffer i (string-ref str-target i))
    (tupper (string->number str-buffer))))
