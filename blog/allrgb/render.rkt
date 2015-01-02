#lang racket

(provide render
         debug:render)

(require images/flomap
         "debug.rkt"
         "pixel-orders.rkt")

(define debug:render (make-parameter #f))
         
; Given a color and order producer, make an image
(define (render width height rgb-producer order-producer)
  ; Initialize the image to error pixels
  (define output (make-vector (* width height) (void)))
  
  ; Access a pixel, will be given to order producer
  ; If default is set, return that on unset or out of bounds
  (define (get-pixel p [default (void)])
    (match-define (pt x y) p)
    (cond
      [(and (>= x 0) (< x width)
            (>= y 0) (< y height))
       (define v (vector-ref output (+ x (* y width))))
       (cond 
         [(not (void? v)) v]
         [(not (void? default)) default]
         [else (error 'get-pixel "pixel at ~a,~a undefined, no default specified" x y)])]
      [(not (void? default))
       default]
      [else
       (error 'get-pixel "invalid coordinates ~a,~a (size = ~a,~a)" x y width height)]))
      
  ; Set a pixel in the final image, error on out of bounds
  (define (set-pixel! p c)
    (match-define (pt x y) p)
    (cond
      [(and (>= x 0) (< x width)
            (>= y 0) (< y height))
       (vector-set! output (+ x (* y width)) c)]
      [else 
       (error 'get-pixel "invalid coordinates ~a,~a (size = ~a,~a)" x y width height)]))
  
  (define count (* width height))
  
  ; Accessor for within the location producer to get the next color
  (define current-color #f)
  (define (get-current-color) current-color)
  
  ; Create the producers, cannot directly use for since we need to set the current color
  (define next-rgb (rgb-producer count))
  (define next-location (order-producer width height get-current-color get-pixel))
  
  ; Build all of the pixels, assume the producers give us enough values
  (define debug:name (~a (or (object-name order-producer) "unknown") "_"
                         (or (object-name rgb-producer) "unknown") "_"
                         width "x" height))
  (for ([index (in-range count)])
    (when (debug:render) (debug debug:name index count))
    (define color (next-rgb))
    (set! current-color color)
    (define location (next-location))
    (set-pixel! location color))
  
  ; Generate the resulting image, setting errors to magenta
  (flomap->bitmap
   (build-flomap* 
    4 width height 
    (λ (x y) (get-pixel (pt x y) (vector 1 1 0 1))))))