#lang racket

(provide recolor/swap
         error/rgb-distance
         error/rgb-distance/average)

(require images/flomap
         math/flonum
         "util.rkt")

; A color with a location
(struct pixel (x y c) #:transparent)

; Find the distance between two flvectors 
(define (fl-distance fl1 fl2)
  (sqrt (for/sum ([a (in-vector (flvector->vector fl1))]
                  [b (in-vector (flvector->vector fl2))])
          (sqr (- a b)))))

; Simple error function based on RGB distance
(define (error/rgb-distance original-fm target-fm color-pixel location-pixel)
    (match-define (pixel x y _) location-pixel)
    (match-define (pixel _ _ c) color-pixel)
    (fl-distance (flomap-ref* original-fm x y) c))

; Slightly more complicated error function takes original image average into account
(define (error/rgb-distance/average original-fm target-fm color-pixel location-pixel)
  (match-define (pixel x y _) location-pixel)
  (match-define (pixel _ _ c) color-pixel)
  (fl-distance (flvector-scale
                (foldl flvector+
                       (flomap-ref* original-fm x y)
                       (for*/list ([xd (in-range -1 2)] [yd (in-range -1 2)])
                         (flomap-ref* original-fm (+ x xd) (+ y yd))))
                (/ 1.0 9.0))
               c))

; Recolor an image by randomly swapping pixels based on improving error
(define (recolor/swap original-src target-src
                      #:threshold [threshold 100]
                      #:error-function [err error/rgb-distance]
                      #:debug-every [debug-every #f])
  
  (define original (load-flomap original-src))
  (define target (load-flomap target-src))
  
  (define width (flomap-width original))
  (define height (flomap-height original))
  
  ; Generate a 2d vector of pixels
  (define pixels
    (list->vector 
     (shuffle
      (for*/list ([y (in-range (flomap-height target))]
                  [x (in-range (flomap-width target))])
        (flomap-ref* target x y)))))
  
  ; Get/set a pixel in the pixel map
  (define (get x y) 
    (pixel x y (vector-ref pixels (+ x (* width y)))))
  
  (define (set! x y c) 
    (vector-set! pixels (+ x (* width y)) c))
  
  ; Get a random pixel (sized from the original image)
  (define (rnd) (get (random width) (random height)))
  
  ; Keep swapping pixels until we get a certain number of non-swaps in a row
  (let loop ([swap-count 0] [non-swap-count 0])
    (define p1 (rnd))
    (define p2 (rnd))
       
    (cond
      ; Haven't swapped recently, return the result
      [(>= non-swap-count threshold)
       (flomap->bitmap 
        (build-flomap*
         (flomap-components original)
         (flomap-width original) (flomap-height original) 
         (λ (x y)
           (pixel-c (get x y)))))]
      ; Swap is better, swap and reset count
      [(< (+ (err original target p1 p2) (err original target p2 p1))
          (+ (err original target p1 p1) (err original target p2 p2)))
       
       (when (and debug-every (zero? (remainder swap-count debug-every)))
         (make-directory* "recolor-swap-debug")
         (define filename (~a (~a swap-count #:width 8 #:align 'right #:pad-string "0") ".png"))
         (displayln filename)
         
         (send 
          (flomap->bitmap 
           (build-flomap*
            (flomap-components original)
            (flomap-width original) (flomap-height original) 
            (λ (x y)
              (pixel-c (get x y)))))
          save-file
          (build-path "recolor-swap-debug" filename)
          'png))
       
       (set! (pixel-x p1) (pixel-y p1) (pixel-c p2))
       (set! (pixel-x p2) (pixel-y p2) (pixel-c p1))
       (loop (+ swap-count 1) 0)]
      ; Swap is worse, just count
      [else
       (loop swap-count (+ non-swap-count 1))])))