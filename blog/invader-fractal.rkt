#lang racket

(require images/flomap
         "procedural-invaders.rkt")

; An invader fractal is nested 5x5 2d vectors, each element is either
; #t/#f - a white/black region
; a recursive 5x5 structure
(define (make-invader-fractal depth)
  (let loop ([depth depth])
    (define invader (flomap-add-margin (procedural-invader (random 32768)) 1))
    (for/vector ([x (in-range 7)])
      (for/vector ([y (in-range 7)])
        (if (> 0.5 (flomap-ref invader 0 x y))
            (if (<= depth 1)
                #t
                (loop (- depth 1)))
            #f)))))

; An invader fractal is nested 5x5 2d vectors, each element is either
; #t/#f - a white/black region
; a recursive 5x5 structure
(define (make-invader-fractal/seeded seed depth)
  (define (mod-random-seed! i)
    (random-seed (+ 1 (remainder i 2147483646))))
  
  (mod-random-seed! seed)
  (let loop ([d depth])
    (define invader (flomap-add-margin (procedural-invader (random 32768)) 1))
    (for/vector ([x (in-range 7)])
      (for/vector ([y (in-range 7)])        
        (if (> 0.5 (flomap-ref invader 0 x y))
            (if (<= d 1)
                #t
                (begin
                  (mod-random-seed! (+ (* (- depth d) 49) (* x 7) y seed))
                  (loop (- d 1))))
            #f)))))

; Render an invader fractal as defined above
; Crop off the margin on the outmost layer
; Final size will be 5*7^{depth-1}
(define (render-invader-fractal fi)
  (define depth
    (let loop ([fi fi])
      (cond
        [(boolean? fi) 0]
        [else 
         (+ 1 (for*/fold ([deepest 0]) ([col (in-vector fi)]
                                        [el (in-vector col)])
                (max (loop el) deepest)))])))
  
  (define size (expt 7 depth))
  
  (flomap->bitmap
   (flomap-crop 
    (build-flomap*
     1 size size
     (λ (x y)
       (let loop ([t 0] [l 0] [s size] [fi fi])
         (cond
           [(eq? fi #t) '#(0.0)]
           [(eq? fi #f) '#(1.0)]
           [else
            ; xi and yi are the points within the current level invader
            (define xi (quotient (* 7 (- x l)) s))
            (define yi (quotient (* 7 (- y t)) s))
            (loop (+ t (* yi (/ s 7)))
                  (+ l (* xi (/ s 7)))
                  (/ s 7)
                  (vector-ref (vector-ref fi xi) yi))]))))
    (* size 5/7)
    (* size 5/7)
    1/2
    1/2)))

(define (invader-fractal seed depth)
  (render-invader-fractal (make-invader-fractal/seeded seed depth)))
