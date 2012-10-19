; fill an rs x cs by coloring each point based on:
; the color in cls associated with the closest point in pts
; calculate all of the distances, sort for smallest, use that one
(define (fill dist rs cs pts cls)
  (make-image rs cs
    (lambda (r c)
      (cadar
        (sort
          (lambda (pc1 pc2) (< (car pc1) (car pc2)))
          (map (lambda (pt cl)
                 (list
                       (dist (list r c) pt) cl)) pts cls))))))

; distance function
(define (distance p1 p2)
  (expt (+ (expt (- (car p1) (car p2)) 2)
           (expt (- (cadr p1) (cadr p2)) 2))
    0.5))

; manhattan distance function
(define (manhattan p1 p2)
  (+ (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))))

; take a quartic instead of a square
(define (quartic p1 p2)
  (expt (+ (expt (- (car p1) (car p2)) 4)
           (expt (- (cadr p1) (cadr p2)) 4))
    0.25))

; a very special division that can divide by 0 :)
(define (/0 a b) (if (= b 0) 1e9 (/ a b)))
 
; use some strange negative power
(define (inverse p1 p2)
  (+ (/0 1 (abs (- (car p1) (car p2)))) (/0 1 (abs (- (cadr p1) (cadr p2))))))
 
; negative distance
(define (ndist p1 p2)
  (- (distance p1 p2)))

; multiply the differences
(define (mul-xy p1 p2)
  (* (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))))

; minimum of the differences
(define (min-xy p1 p2)
  (min (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))))

; max of the differences
(define (max-xy p1 p2)
  (max (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))))

; a faster fill function
(define (fill-fast dist rs cs pts cls)
  (make-image rs cs
    (lambda (r c)
      (let ([rc (list r c)])
        (let loop ([min-d (dist (car pts) rc)]
                   [min-cl (car cls)]
                   [pts (cdr pts)]
                   [cls (cdr cls)])
          (if (null? pts)
              min-cl
              (let ([new-d (dist (car pts) rc)])
                (if (< new-d min-d)
                    (loop new-d (car cls) (cdr pts) (cdr cls))
                    (loop min-d min-cl (cdr pts) (cdr cls))))))))))

; a fill function using fold
(define (fill-fold dist rs cs pts cls)
  (make-image rs cs
    (lambda (r c)
      (let ([rc (list r c)])
        (fold-left
          (lambda (min-d/cl pt cl)
            (let ([new-d (dist (car pts) rc)])
              (if (< new-d (car min-d/cl))
                  (list new-d cl)
                  min-d/cl)))
          (list (dist (car pts) rc) (car cls))
          (cdr pts)
          (cdr cls))))))

; a faster distance function
(define (fast-distance p1 p2)
  (let ([x (- (car p1) (car p2))]
        [y (- (cadr p1) (cadr p2))])
    (+ (* x x) (* y y))))

; test framework
(define (test rs cs n)
  (let* ([pts (map (lambda (_) (list (random rs) (random cs))) (iota n))]
         [cls (map (lambda (_) (random-color)) (iota n))])
    (timed as "original" (fill distance rs cs pts cls))
    (timed as "faster" (fill-fast distance rs cs pts cls))
    (timed as "folded (left)" (fill-fold distance rs cs pts cls))
    (timed as "folded (right)" (fill-fold-right distance rs cs pts cls))
    (timed as "original w/ fast dist" (fill fast-distance rs cs pts cls))
    (timed as "faster w/ fast dist" (fill-fast fast-distance rs cs pts cls))
    (timed as "folded (left) w/ fast dist" (fill-fold fast-distance rs cs pts cls))
    (timed as "folded (right) w/ fast dist" (fill-fold-right fast-distance rs cs pts cls))
    (void)))