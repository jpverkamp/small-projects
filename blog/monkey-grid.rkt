#lang racket

; ----- infinite-grid ADT -----

(require images/flomap)

(define (make-infinite-grid)
  (make-hasheq))

(define (infinite-grid-ref grid x y [default (void)])
  (hash-ref (hash-ref! grid x (make-hasheq)) y default))

(define (infinite-grid-set! grid x y value)
  (hash-set! (hash-ref! grid x (make-hasheq)) y value))

(define (infinite-grid-range grid)
  (values (apply min (cons 0 (hash-keys grid)))
          (apply max (cons 0 (hash-keys grid)))
          (apply min (cons 0 (map (λ (hash) (apply min (cons 0 (hash-keys hash)))) (hash-values grid))))
          (apply max (cons 0 (map (λ (hash) (apply max (cons 0 (hash-keys hash)))) (hash-values grid))))))

(define (infinite-grid->bitmap grid mapping [default (void)])
  (let-values ([(min-x max-x min-y max-y) (infinite-grid-range grid)])
    (flomap->bitmap
     (build-flomap* 
      3 (- max-x min-x) (- max-y min-y)
      (λ (x y)
        (mapping (infinite-grid-ref grid (+ x min-x) (+ y min-y) default)))))))

(define (infinite-grid-keys grid)
  (for*/list ([x (in-list (hash-keys grid))]
              [y (in-list (hash-keys (hash-ref grid x)))])
    (list x y)))

(define (infinite-grid-values grid)
  (for*/list ([x (in-list (hash-keys grid))]
              [y (in-list (hash-keys (hash-ref grid x)))])
    (infinite-grid-ref grid x y)))
                         
; ----- monkey-grid -----

(define (monkey-grid rule [maximum-steps #f] #:depth-first [depth-first #t])
  ((if depth-first monkey-grid-df monkey-grid-bf) rule maximum-steps))

(define (monkey-grid-df rule [maximum-steps #f])
  (define grid (make-infinite-grid))
  (define steps (make-parameter 0))
  (let loop ([x 0] [y 0])
    (cond
      ; We're out of steps or points
      [(and (number? maximum-steps) (>= (steps) maximum-steps))
       (void)]
      ; We've already visited this point, skip it
      [(infinite-grid-ref grid x y #f)
       (void)]
      ; The monkey can walk here
      [(rule x y)
       (steps (+ (steps) 1))
       (infinite-grid-set! grid x y (steps))
       (loop (- x 1) y) 
       (loop (+ x 1) y) 
       (loop x (- y 1))
       (loop x (+ y 1))]
      ; The monkey cannot walk here, skip it
      [else
       (infinite-grid-set! grid x y #f)]))
  grid)

(define (monkey-grid-bf rule [maximum-steps #f])
  (define grid (make-infinite-grid))
  (define steps (make-parameter 0))
  (let/ec return
    (let loop ([points '((0 0))])
      ; We're out of steps or points
      (when (or (null? points)
                (and (number? maximum-steps) (>= (steps) maximum-steps)))
        (return grid))
      
      ; Otherwise, unpack and process the next point
      (define x (first (first points)))
      (define y (second (first points)))
      (cond
        ; We've already visited this point, skip it
        [(infinite-grid-ref grid x y #f)
         (loop (rest points))]
        ; The monkey can walk here
        [(rule x y)
         (steps (+ 1 (steps)))
         (infinite-grid-set! grid x y (steps))
         (loop (append (rest points)
                       (list (list (- x 1) y) 
                             (list (+ x 1) y) 
                             (list x (- y 1))
                             (list x (+ y 1)))))]
        ; The monkey cannot walk here, skip it
        [else
         (infinite-grid-set! grid x y #f)
         (loop (rest points))]))))

; ----- coloring functions -----

(define (default-coloring value)
  (cond
    [(number? value) (vector 1.0 0.0 0.0)]
    [(not value)     (vector 0.0 0.0 0.0)]
    [else            (vector 1.0 1.0 1.0)]))

(define (make-gradient-coloring grid)
  (define max-value (apply max (cons 1 (filter number? (infinite-grid-values grid)))))
  (λ (value)
    (cond
      [(number? value) 
       (define g (/ value max-value))
       (vector g 0 (- 1 g))]
      [(not value) (vector 0.0 0.0 0.0)]
      [else        (vector 1.0 1.0 1.0)])))

; ----- examples -----

(define (make-grid default-param function)
  (λ (              [param default-param]
                    #:steps       [steps (expt 2 15)]
                    #:depth-first [depth-first #t]
                    #:gradient    [gradient    #f])
    
    (define grid
      (monkey-grid
       (function param)
       steps
       #:depth-first depth-first))
    
    (infinite-grid->bitmap
     grid
     (if gradient (make-gradient-coloring grid) default-coloring))))

(define monkey
  (let ([digits 
         (λ (n)
           (let loop ([n n] [ls '()])
             (cond
               [(= n 0) ls]
               [else    (loop (quotient n 10) (cons (remainder n 10) ls))])))])
    (make-grid 
     19
     (λ (max-digit-sum) 
       (λ (x y) (<= (apply + (append (digits (abs x)) (digits (abs y)))) max-digit-sum))))))


(define squiggle 
  (make-grid 
   2
   (λ (variation) 
     (λ (x y) 
       (positive? (random variation))))))

(define circle
  (make-grid
   50
   (λ (radius)
     (define r^2 (* radius radius))
     (λ (x y) 
       (<= (+ (* x x) (* y y)) r^2)))))

(define square
  (make-grid
   50
   (λ (radius)
     (λ (x y) 
       (and (<= (abs x) radius) (<= (abs y) radius))))))

(require math/number-theory)
(define either-prime
  (make-grid
   +inf.0
   (λ (bound)
     (λ (x y)
       (and (<= (abs x) bound) (<= (abs y) bound)
            (or (<= -2 x 2) (<= -2 y 2)
                (prime? x)  (prime? y)))))))

(define coprime
  (make-grid
   +inf.0
   (λ (bound)
     (λ (x y)
       (and (<= (abs x) bound) (<= (abs y) bound)
            (or (<= -2 x 2) (<= -2 y 2) (coprime? x y)))))))
