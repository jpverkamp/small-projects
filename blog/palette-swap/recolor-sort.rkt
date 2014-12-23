#lang racket

(provide recolor/sort
         pixel<?/grayscale
         make-pixel<?/grayscale/weighted)

(require images/flomap
         math/flonum
         "util.rkt")

; A color with a location
(struct pixel (x y c) #:transparent)

; Sorting function based on grayscale value
(define (pixel<?/grayscale p1 p2)
    (< (apply + (flvector->list (pixel-c p1)))
       (apply + (flvector->list (pixel-c p2)))))

; Grayscale values, only this time 
(define (make-pixel<?/grayscale/weighted weights)
  (λ (p1 p2)
    (< (apply + (flvector->list (flvector* weights (pixel-c p1))))
       (apply + (flvector->list (flvector* weights (pixel-c p2)))))))

; Recolor an image by sorting the pixels in both images
(define (recolor/sort original-src target-src #:pixel<? [pixel<? pixel<?/grayscale])
  (define original (load-flomap original-src))
  (define target (load-flomap target-src))
  
  ; Generate a list of pixels in each image
  (define (pixel-list fm)
    (for*/list ([x (in-range (flomap-width fm))]
                [y (in-range (flomap-height fm))])
      (pixel x y (flomap-ref* fm x y))))
  
  (define original-pixels (pixel-list original))
  (define target-pixels (pixel-list target))
  
  ; Sort both lists by the given sorting function
  (define sorted-original-pixels (sort original-pixels pixel<?))
  (define sorted-target-pixels (sort target-pixels pixel<?))
  
  ; Build a map from source xy to list index to target color
  (define transition-hash
    (for/fold ([h (hash)])
              ([original-pixel (in-list sorted-original-pixels)]
               [target-pixel (in-list sorted-target-pixels)])
      (hash-set h
                (list (pixel-x original-pixel)
                      (pixel-y original-pixel))
                (pixel-c target-pixel))))
  
  ; Build the new image from that transition matrix
  (flomap->bitmap 
   (build-flomap*
    (flomap-components original)
    (flomap-width original) (flomap-height original) 
    (λ (x y)
      (hash-ref transition-hash (list x y))))))