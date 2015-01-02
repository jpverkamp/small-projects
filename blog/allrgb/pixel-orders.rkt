#lang racket

(provide list-order-producers
         get-order-producer
         in-order-producers
         (struct-out pt))

(require racket/generator
         "colors.rkt")

; Structured points, each producer produces these
(struct pt (x y) #:prefab)

(define (pt+ p1 p2)
  (match-define (pt x1 y1) p1)
  (match-define (pt x2 y2) p2)
  (pt (+ x1 x2) (+ y1 y2)))

; Interface to export all known order producers 
(define order-producers (make-hash))

(define (list-order-producers) (hash-keys order-producers))
(define (get-order-producer name) (hash-ref order-producers name))
(define (in-order-producers) (in-hash order-producers))

(define-syntax-rule (define-order-producer (name width height get-new-color get-color-at) body ...)
  (let ()
    (define (name width height get-new-color get-color-at) body ...)
    (hash-set! order-producers 'name name)))

; ===== ===== ===== ===== =====

; Left to right, top to bottom
(define-order-producer (reading width height get-new-color get-color-at)
  (generator ()
    (for* ([y (in-range height)] [x (in-range width)])
      (yield (pt x y)))))

; Spiral from the center point outwards 
(require data/queue)

(define-order-producer (spiral width height get-new-color get-color-at)
  (define q (make-queue))
  (enqueue! q (pt (quotient width 2) (quotient height 2)))
  
  (define (in-bounds? p)
    (match-define (pt x y) p)
    (and (>= x 0) (< x width)
         (>= y 0) (< y height)))
         
  (λ ()
    (let loop ()
      (define next (dequeue! q))
      (cond
        [(or (not (in-bounds? next)) (get-color-at next #f)) (loop)]
        [else 
         (match-define (pt x y) next)
         (for* ([xd (in-range -1 2)] [yd (in-range -1 2)])
           (enqueue! q (pt (+ x xd) (+ y yd))))
         next]))))

; Generate random squares each until they hit an edge or another square 
(define-order-producer (square-fill width height get-new-color get-color-at)
  (define q (make-queue))
  (enqueue! q (pt (random width) (random height)))
  
  (define this-square (make-hash))
  
  (define (in-bounds? p)
    (match-define (pt x y) p)
    (and (>= x 0) (< x width)
         (>= y 0) (< y height)))
         
  (λ ()
    (let loop ()
      (define next (dequeue! q))
      (cond
        ; Already set, try again with this square
        [(hash-has-key? this-square next)
         (loop)]
        ; Out of bounds or already set, seed a new square
        [(or (not (in-bounds? next)) 
             (get-color-at next #f))

         (queue-filter! q (const #f))
         (hash-clear! this-square)
         
         (enqueue! q (pt (random width) (random height)))
         (loop)]
        ; Otherwise, return that point and queue up its neighbors
        [else 
         (match-define (pt x y) next)
         (for* ([xd (in-range -1 2)] [yd (in-range -1 2)])
           (enqueue! q (pt (+ x xd) (+ y yd))))
         (hash-set! this-square next #t)
         next]))))

; Walk randomly, starting a new seed if there are no more options
(define-order-producer (random-walk width height get-new-color get-color-at)
  (define current-point (pt (random width) (random height)))
  
  (define (in-bounds? p)
    (match-define (pt x y) p)
    (and (>= x 0) (< x width)
         (>= y 0) (< y height)))
         
  (λ ()
    (begin0
      current-point
     
      (let ()
        (match-define (pt x y) current-point)
        (define next-point
          (or 
           ; Find a neighboring point that hasn't been set
           (for*/first ([xd (in-list (shuffle (range -1 2)))]
                        [yd (in-list (shuffle (range -1 2)))]
                        [neighbor (in-value (pt (+ x xd) (+ y yd)))]
                        #:when (and (in-bounds? neighbor)
                                    (not (= xd yd 0))
                                    (not (get-color-at neighbor #f))))
             neighbor)
           ; If that doesn't work, generate a new seed
           (let loop ()
             (define new-seed (pt (random width) (random height)))
             (if (get-color-at new-seed #f)
                 (loop)
                 new-seed))))
        
        (set! current-point next-point)))))

; Organically grow outwards, finding the best match among border pixels

; combine is how distances of border pixels are combined
; missing is the initial value and the value used if a point is missing
; after is called once with the final value (defaults to identity)
(define (make-grower #:combine combine 
                     #:missing missing 
                     #:after [after identity]
                     #:seeds [seeds #f])
  (λ (width height get-new-color get-color-at)
    (define border (make-hash))
    (if seeds
        (for-each (λ (seed) (hash-set! border seed #t)) seeds)
        (hash-set! border (pt (quotient width 2) (quotient height 2)) #t))
    
    (λ ()
      (define new-color (get-new-color))
      
      ; Loop through all border pixels to find the best match
      (define-values (_ best-point)
        (for/fold ([best-distance +inf.0] [best-point #f])
                  ([(point _) (in-hash border)])
          
          ; Find the minimum distance to pixels adjacent to that border
          (define new-distance
            (after
             (for*/fold ([new-distance missing])
                        ([xd (in-range -1 2)] [yd (in-range -1 2)])
               (define c (get-color-at (pt+ point (pt xd yd)) #f))
               (combine new-distance (if c (rgb-distance new-color c) missing)))))
          
          (if (<= new-distance best-distance)
              (values new-distance point)
              (values best-distance best-point))))
      
      ; Update the borders without that point but with it's neighbors
      (hash-remove! border best-point)
      (match-define (pt x y) best-point)
      
      (for* ([xd (in-range -1 2)] 
             [yd (in-range -1 2)]
             [new-border-point (in-value (pt (+ x xd) (+ y yd)))]
             #:when (and (not (= xd yd 0))
                         (>= (+ x xd) 0) (< (+ x xd) width)
                         (>= (+ y yd) 0) (< (+ y yd) height)
                         (not (get-color-at new-border-point #f))))
        (hash-set! border new-border-point #t))
      
      best-point)))

(define (corners width height)
  (list (pt 0 0)
        (pt 0 (- height 1))
        (pt (- width 1) 0)
        (pt (- width 1) (- height 1))))

; Choose the border pixel closest to any one of its neighbors
(define-order-producer (grow-minimum width height get-new-color get-color-at)
  ((make-grower #:combine min #:missing +inf.0)
   width height get-new-color get-color-at))

; Choose the border pixel closest to the furthest away neighbor
(define-order-producer (grow-maximum width height get-new-color get-color-at)
  ((make-grower #:combine max #:missing -inf.0)
   width height get-new-color get-color-at))

(define-order-producer (grow-maximum-corners width height get-new-color get-color-at)
  ((make-grower #:combine max #:missing -inf.0 #:seeds (corners width height))
   width height get-new-color get-color-at))

; Choose the border pixel where the difference from the average of already placed pixels is minimal
(define-order-producer (grow-average width height get-new-color get-color-at)
  ; Sneaky trick to track both sum (real) and count (imagionary) 
  ((make-grower #:combine (λ (old-v new-v) (+ old-v new-v 0+1i))
                #:missing 0
                #:after (λ (v) (if (= v 0) 0 (/ (real-part v) (imag-part v)))))
   width height get-new-color get-color-at))
