#lang racket

(provide load-flomap
         make-allrgb)

(require images/flomap
         (only-in racket/draw read-bitmap bitmap%))

; Load various kinds of things into a flomap
(define (load-flomap thing)
  (cond
    [(or (string? thing)
         (path? thing))
     (bitmap->flomap (read-bitmap thing))]
    [(is-a? thing bitmap%) (bitmap->flomap thing)]
    [(flomap? thing) thing]))

; Generate an image containing unique pixels uniformly selected from the RGB color space
; An image with 16777216 will contain all RGB values
(define (make-allrgb width height #:channels [channels 4])
  (build-flomap*
   channels width height
   (λ (x y)
     (define i (if (= x y 0) 0 (/ (* width height) (+ x (* width y)))))
     (define n (inexact->exact (floor (* i 256 256 256))))
     (define r (/ (remainder n 256) 256.0))
     (define g (/ (remainder (quotient n 256) 256) 256.0))
     (define b (/ (remainder (quotient n (* 256 256)) 256) 256.0))
     (vector 1.0 r g b))))
