#lang racket

(provide (all-defined-out))

(require images/flomap
         "procedural-invaders.rkt")

(struct rect (t l w h) #:transparent)
(struct node (bounds value-bounds value children) #:transparent)

(define (in? bounds x y)
  (match-define (rect t l w h) bounds)
  (and (<= l x (+ l w -1))
       (<= t y (+ t h -1))))

; Create the recursive fractal structure
(define (make-fractal width height random-node #:maximum-block-size [max-size #f])
  (let loop ([t 0] [l 0] [w width] [h height])
    (cond
      ; The next node is too small, do not place it
      [(or (<= w 0) (<= h 0) (>= t height) (>= l width))
       #f]
      ; Create a child node; recur four times as so:
      ;  T  |
      ; __  |
      ;   XX R
      ; L XX__
      ;   |
      ;   | B
      [else 
       (define s
         (let loop ()
           (define s (if (= (min w h) 1) 1 (+ 1 (random (min w h)))))
           (cond
             [(or (not max-size) (< s max-size)) s]
             [else (loop)])))
       
       (define x (if (= w s)         0 (random (- w s))))
       (define y (if (= h s)         0 (random (- h s))))
       (node (rect t       l       w h) ; Bounds of this node
             (rect (+ t y) (+ l x) s s) ; Bounds of the value within this node
             (random-node)              ; The value of this node
             (list (loop t         l         (+ x s)   y        )
                   (loop t         (+ l x s) (- w x s) (+ y s)  )
                   (loop (+ t y s) (+ l x)   (- w x)   (- h y s))
                   (loop (+ t y)   l         x         (- h y)  )))])))

; Render a fractal image
(define (fractal-image
         width height
         #:random-color [random-color (thunk (vector (random) (random) (random)))])
  
  (define root (make-fractal width height random-color))
  
  (flomap->bitmap
   (build-flomap*
    3 width height
    (λ (x y)
      (let loop ([node root])
        (cond
          [(in? (node-value-bounds node) x y) (node-value node)]
          [else
           (for*/first ([child (in-list (node-children node))]
                        #:when (and child (in? (node-bounds child) x y)))
             (loop child))]))))))

; Render a fractal image made of invaders!
(define (fractal-invaders width height #:highlights? [highlights? #f] #:maximum-invader-size [max-size #f])
  (define (random-invader) 
    (flomap-add-margin 
     (if highlights?
         (procedural-invader/highlight (random 524288))
         (procedural-invader (random 32768)))
     1))
        
  (define root 
    (make-fractal 
     (quotient width 7) 
     (quotient height 7)
     random-invader
     #:maximum-block-size (and max-size (/ max-size 7))))
  
  (flomap->bitmap
   (build-flomap*
    (if highlights? 3 1) width height
    (λ (x y)
      ; Correct for coordinates within the node
      (define nx (quotient x 7)) 
      (define ny (quotient y 7))
      
      (let loop ([n root])
        (cond
          [(in? (node-value-bounds n) nx ny)
           ; Calculate coordinates within the image
           (match-define (node _ (rect t l s _) img _) n)
           (define ix (quotient (- x (* 7 l)) s))
           (define iy (quotient (- y (* 7 t)) s))
           (flomap-ref* img ix iy)]
          [else
           (or 
            (for*/first ([child (in-list (node-children n))]
                         #:when (and child (in? (node-bounds child) nx ny)))
              (loop child))
            (if highlights? '#(1 1 1) '#(1)))]))))))

#|
; Random image
> (fractal-image 200 200)

; Random grayscale image
> (fractal-image 200 200 #:random-color (thunk (let ([g (random)]) (vector g g g))))

; Random RGB image
> (fractal-image 200 200 
                 #:random-color (thunk 
                                  (case (random 3)
                                    [(0) (vector (random) 0 0)]
                                    [(1) (vector 0 (random) 0)]
                                    [(2) (vector 0 0 (random))])))
|#