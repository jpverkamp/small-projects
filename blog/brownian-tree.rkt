#lang racket

; https://en.wikipedia.org/wiki/Brownian_tree

(require 2htdp/image
         2htdp/universe
         (only-in racket/draw the-color-database))

(struct point (x y c) #:transparent)
(struct world (radius scale points drip) #:transparent)

(define current-wiggle-real? (make-parameter #f))
(define current-spacing (make-parameter (sqrt 2)))
(define current-inward-bias (make-parameter 0.5))

; Distance formula
(define (δ pt1 pt2)
  (sqrt (+ (sqr (- (point-x pt1) (point-x pt2)))
           (sqr (- (point-y pt1) (point-y pt2))))))

; List of color names from racket/draw
(define colors
  (list->vector
    (send the-color-database get-names)))

; Generate a new random point on the border of a given circle
(define (random-point r)
  (define θ (* 2 pi (random)))
  (point (inexact->exact (truncate (* r (cos θ))))
         (inexact->exact (truncate (* r (sin θ))))
         (vector-ref colors (random (vector-length colors)))))

; The origin point (for distance comparisons and starting the simulation)
(define origin (point 0 0 (color 0 0 0 255)))

; Wiggle a point closer to the origin, sticking to the unit grid
(define (wiggle:grid pt)
  (let ([max-d (δ origin pt)])
    (let loop ()
      (define xδ? (zero? (random 2)))
      (define new-pt
        (point (if xδ? (+ (point-x pt) (random 3) -1) (point-x pt))
               (if xδ? (point-y pt) (+ (point-y pt) (random 3) -1))
               (point-c pt)))
      (if (or (<= (δ origin new-pt) max-d) (< (current-inward-bias) (random)))
          new-pt
          (loop)))))

; Wiggle a point closer to the origin, returning any new point within the unit circle
(define (wiggle:real pt)
  (let ([max-d (δ origin pt)])
    (let loop ()
      (define θ (* 2 pi (random)))
      (define new-pt
        (point (+ (point-x pt) (cos θ))
               (+ (point-y pt) (sin θ))
               (point-c pt)))
      (if (or (<= (δ origin new-pt) max-d) (< (current-inward-bias) (random)))
          new-pt
          (loop)))))

; Either randomly move the drip or create a new one
(define (update w)
  ; Always try to move the drip first
  (define new-drip
    ((if (current-wiggle-real?) wiggle:real wiggle:grid) 
     (world-drip w)))
  
  (cond
    ; If it's adjacent to any current point, freeze it and generate a new drip
    [(ormap (λ (pt) (<= (δ new-drip pt) (current-spacing)))
            (world-points w))
     
     (world (world-radius w)
            (world-scale w)
            (cons new-drip (world-points w))
            (random-point (world-radius w)))]
    ; Otherwise, just use the new drip
    [else
     (world (world-radius w)
            (world-scale w)
            (world-points w)
            new-drip)]))

; Faster version of update that loops until the new point freeze into place
(define (fast-update w)
  (let loop ([w^ w])
    (if (eq? (world-points w) (world-points w^))
        (loop (update w^))
        w^)))

; Helper to draw a circle with a black border
(define (outlined-circle size color)
  (overlay (circle size "outline" "black")
           (circle size "solid" color)))

; Draw the current points in an image
(define (draw w)
  (for/fold ([img (circle (* (world-scale w) (world-radius w)) "outline" "black")]) 
            ([pt (in-list (cons (world-drip w)
                                (world-points w)))])
    (overlay/offset (outlined-circle (/ (world-scale w) 2) (point-c pt))
                    (* (world-scale w) (point-x pt))
                    (* (world-scale w) (point-y pt))
                    img)))

; Create a cached draw function for a particular world
(define (make-cached-draw initial-world)
  (let ([cache-key '()] [cache-val (circle (* (world-scale initial-world) (world-radius initial-world)) "outline" "black")])
    (λ (w)
      ; If we have at least one new point, add all new points to the cached image
      (when (not (eq? cache-key (world-points w)))
        (set! cache-val
          (for/fold ([img cache-val]) ([pt (in-list (world-points w))]
                                       #:break (member pt cache-key))
            (overlay/offset (outlined-circle (/ (world-scale w) 2) (point-c pt))
                            (* (world-scale w) (point-x pt))
                            (* (world-scale w) (point-y pt))
                            img)))
        (set! cache-key (world-points w)))
      ; Add the drip (never cached)
      (overlay/offset (outlined-circle (/ (world-scale w) 2) (point-c (world-drip w)))
                      (* (world-scale w) (point-x (world-drip w)))
                      (* (world-scale w) (point-y (world-drip w)))
                      cache-val))))

; Run the full simulation, returning the resulting image
(define (brownian-tree radius 
                       #:scale     [scale 1.0]
                       #:bias      [bias 0.5]
                       #:spacing   [spacing 1]
                       #:fast-mode [fast-mode? #f] 
                       #:real-mode [real-mode? #f])
  (parameterize ([current-wiggle-real? real-mode?]
                 [current-spacing spacing]
                 [current-inward-bias bias])
    (define initial-world (world radius scale (list origin) (random-point radius)))
    (define draw (make-cached-draw initial-world))
    (draw
     (big-bang initial-world
       (on-tick (if fast-mode? fast-update update))
       (to-draw draw)))))
