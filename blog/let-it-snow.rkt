#lang racket/base

; https://codegolf.stackexchange.com/questions/42506/draw-a-snowflake

(require 2htdp/universe
         images/flomap
         racket/match)

(define  π 3.141592653589793)
(define 2π (* 2 π))

(struct snowflake (radius arms points) #:prefab)

; Rotate a point so that the angle is in the range [θ, -θ)
(define (clampθ p θmin θmax)
  (define θΔ (- θmax θmin))
  (make-polar 
   (magnitude p)
   (let loop ([θ (if (= 0 p) 0 (angle p))])
     (cond
       [(<  θ θmin) (loop (+ θ θΔ))]
       [(>= θ θmax) (loop (- θ θΔ))]
       [else        θ]))))

; Generate a new snowflake with the given number of arms
(define (make-snowflake radius arms)
  (snowflake radius arms '(0)))

; Test if a snowflake contains a point within a given Δ
(define (snowflake-contains? s p Δ)
  (match-define (snowflake radius arms points) s)
  
  (define θ  (/ 2π arms))
  (define pα (clampθ p 0 θ))
  (define pβ (make-polar (magnitude pα) (if (= pα 0) 0 (- θ (angle pα)))))
  
  (ormap (λ (point) 
           (or (< (magnitude (- pα point)) Δ)
               (< (magnitude (- pβ point)) Δ)))
         points))

; Add a new point to the snowflake
(define (snowflake-set s p)
  (match-define (snowflake radius arms points) s)
  (define θmax (/ 2π arms))
  (snowflake radius arms (cons (clampθ p 0 θmax) points)))

; Generate a new point on the snowflake by drifting a point inwards
(define (snowflake-grow s [Δ (sqrt 2)])
  (match-define (snowflake radius arms points) s)
  
  (let loop ([p (make-polar radius (* (random) 2π))])
    (cond
      ; Hit an already existing point, add it and return
      [(snowflake-contains? s p Δ)
       (snowflake-set s p)]
      ; Otherwise, try to drift inwards
      [else
       (loop 
        (for/fold ([p p])
                  ([step (in-list 
                          (list 
                           ; Drift randomly
                           (λ (p) (+ p (make-polar (* (random) Δ) (* (random) 2π))))
                           ; Drift inwards
                           (λ (p) (make-polar
                                   (- (magnitude p) (* (random) Δ))
                                   (angle p)))))])
          (step p)))])))

; Generate an entire snowflake
(define (generate-snowflake radius arms [Δ (sqrt 2)])
  (let loop ([s (make-snowflake radius arms)]) 
    (define s^ (snowflake-grow s Δ))
    (if (> (magnitude (car (snowflake-points s^))) radius)
        s^
        (loop s^))))

; Render a snowflake to a bitmap
(define (render-snowflake s)
  (match-define (snowflake radius arms data) s)
  (define size (+ 1 (* radius 2)))
  
  (flomap->bitmap
   (build-flomap*
    1 size size
    (λ (x y) 
      (define p (make-rectangular (- x radius) (- y radius)))
      (vector (if (snowflake-contains? s p 1) 1 0))))))

; Render a snowflake being formed
(define (render-snowflake/frames s)
  (match-define (snowflake radius arms data) s)
  
  (define snowflakes
    (reverse
     (for/fold ([s* (list (make-snowflake radius arms))])
               ([pt (in-list (reverse data))])
       (match-define (list* first rest) s*)
       (match-define (snowflake _ _ data^) first)
       (list* (snowflake radius arms (cons pt data^)) s*))))
  
  (map render-snowflake snowflakes))

; ----- ----- ----- ----- ----- 

(require mrlib/gif
         racket/class
         racket/format
         racket/file)

(define (test1)
  (make-directory* (build-path "snowflakes" "test1"))
  (for* ([arms (in-list '(3 4 6 7 8 16))]
         [Δ (in-list '(root2 2 π 4))])
    (define filename (build-path "snowflakes" "test1" (~a arms "_" Δ ".png")))
    (displayln filename)
    
    (define realΔ
      (case Δ
        [(root2) (sqrt 2)]
        [(π)    π]
        [else    Δ]))
    
    (define img (render-snowflake (generate-snowflake 50 arms realΔ)))
    (send img save-file filename 'png)))

(define (test2)
  (make-directory* (build-path "snowflakes" "test2"))
  (for* ([i (in-range 100)])
    (define istr (~a i #:width 3 #:align 'right #:pad-string "0"))
    (define filename (build-path "snowflakes" "test2" (~a istr ".png")))
    (displayln filename)
    
    (define img (render-snowflake (generate-snowflake 50 6 π)))
    (send img save-file filename 'png)))

(define (test3)
  (make-directory* (build-path "snowflakes" "test3"))
  (define filename (build-path "snowflakes" "test3" "snowflake.gif"))
  
  (when (file-exists? filename)
    (delete-file filename))
  
  (write-animated-gif 
   (render-snowflake/frames (generate-snowflake 50 8 4))
   5
   filename
   #:last-frame-delay 50))
