#lang racket

(provide recolor/fill)

(require data/order
         data/splay-tree
         images/flomap
         math/flonum
         racket/generator
         "util.rkt")

; Spiral outwards from (0,0) in squares
(define (in-spiral [radius +inf.0])
  (in-generator
    (yield (list 0 0))
    (for ([r (in-range 1 radius)])
      (yield (list (- r) (- r)))
      (yield (list (- r) r))
      (yield (list r (- r)))
      (yield (list r r))
      (for ([d (in-range (+ (- r) 1) r)])
        (yield (list (- r) d))
        (yield (list r     d))
        (yield (list d (- r)))
        (yield (list d r))))))

; Find the distance between two flvectors 
(define (fl-distance fl1 fl2)
  (sqrt (for/sum ([a (in-vector (flvector->vector fl1))]
                  [b (in-vector (flvector->vector fl2))])
          (sqr (- a b)))))

; Test if two flvectors are equal
(define (flvector=? flv1 flv2)
  (not (for/first ([v1 (in-flvector flv1)]
                   [v2 (in-flvector flv2)]
                   #:when (not (= v1 v2)))
         #t)))

; Test if one flvector is less than another by comparing each channel in order
(define (flvector<? flv1 flv2)
  (for/first ([v1 (in-flvector flv1)]
              [v2 (in-flvector flv2)]
              #:when (not (= v1 v2)))
    (< v1 v2)))

; Recolor an image finding the best pixels from the center out
(define (recolor/fill/slow original-src target-src)
  (define original (load-flomap original-src))
  (define target (load-flomap target-src))
  
  (define width (flomap-width original))
  (define height (flomap-height original))
  
  ; Generate a list of target colors
  (define colors
    (for*/list ([y (in-range (flomap-height target))]
                [x (in-range (flomap-width target))])
      (flomap-ref* target x y)))
  
  ; Generate a 2d vector of pixels
  (define result
    (for*/vector ([y (in-range (flomap-height original))]
                  [x (in-range (flomap-width original))])
      #f))
  
  ; Get/set a pixel in the pixel map
  (define (result-get x y) 
    (vector-ref result (+ x (* width y))))
  
  (define (result-set! x y c) 
    (vector-set! result (+ x (* width y)) c))
  
  ; Spiral outwards from the center of the image
  (for ([pt (in-spiral (+ 2 (quotient (max width height) 2)))])
    ; Convert to image coordinates and verify that we're in the image
    (define x (+ (first pt) (quotient width 2)))
    (define y (+ (second pt) (quotient height 2)))    
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      ; Get the source color at that point
      (define target-color (flomap-ref* original x y))
      
      ; Choose the closest remaining color
      (define-values (_ color)
        (for/fold ([minimum-distance +inf.0] [best-color #f])
                  ([color (in-list colors)])
          (define new-distance (fl-distance target-color color))
          (if (< new-distance minimum-distance)
              (values new-distance color)
              (values minimum-distance best-color))))
          
      ; Remove that color from the list to place, add it to the result
      (set! colors (remove color colors))
      (result-set! x y color)))
  
  ; Turn that into a bitmap
  (flomap->bitmap 
   (build-flomap*
    (flomap-components original)
    (flomap-width original) (flomap-height original) 
    result-get)))

; Recolor an image finding the best pixels from the center out
(define (recolor/fill original-src target-src)
  (define original (load-flomap original-src))
  (define target (load-flomap target-src))
  
  (define width (flomap-width original))
  (define height (flomap-height original))
  
  ; Generate a list of target colors
  (define colors
    (make-splay-tree 
     (order 'pixel-grayscale-order 
            flvector?
            flvector=?
            flvector<?)))

  (for* ([y (in-range (flomap-height target))]
         [x (in-range (flomap-width target))])
    
    (define color (flomap-ref* target x y))
    (define count (+ 1 (splay-tree-ref colors color 0)))
    (splay-tree-set! colors color count))
     
  ; Generate a 2d vector of pixels
  (define result
    (for*/vector ([y (in-range (flomap-height original))]
                  [x (in-range (flomap-width original))])
      #f))
  
  ; Get/set a pixel in the pixel map
  (define (result-get x y) 
    (vector-ref result (+ x (* width y))))
  
  (define (result-set! x y c) 
    (vector-set! result (+ x (* width y)) c))
  
  ; Spiral outwards from the center of the image
  (for ([pt (in-spiral (+ 2 (quotient (max width height) 2)))])
    ; Convert to image coordinates and verify that we're in the image
    (define x (+ (first pt) (quotient width 2)))
    (define y (+ (second pt) (quotient height 2)))    
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      ; Get the source color at that point
      (define target-color (flomap-ref* original x y))
      
      ; Choose the closest remaining color
      (define iter/>= (splay-tree-iterate-least/>=? colors target-color))    
      (define iter/<= (splay-tree-iterate-greatest/<=? colors target-color))
      
      (define color
        (cond
          [(and iter/>= iter/<=)
           (define c1 (splay-tree-iterate-key colors iter/>=))
           (define c2 (splay-tree-iterate-key colors iter/<=))
           (if (< (fl-distance target-color c1)
                  (fl-distance target-color c2))
               c1 
               c2)]
          [iter/>= (splay-tree-iterate-key colors iter/>=)]
          [iter/<= (splay-tree-iterate-key colors iter/<=)]))

      (define count (splay-tree-ref colors color))
      (if (= count 1)
          (splay-tree-remove! colors color)
          (splay-tree-set! colors color (- count 1)))
      
      (result-set! x y color)))
  
  ; Turn that into a bitmap
  (flomap->bitmap 
   (build-flomap*
    (flomap-components original)
    (flomap-width original) (flomap-height original) 
    result-get)))