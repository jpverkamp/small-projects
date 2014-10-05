#lang racket

(require images/flomap)

; Get the maximum path length; useful for making gradients
(define (size->path-length size)
  (inexact->exact (floor (/ (log size) (log 2)))))

; Generate a fractal by matching a recursive path into an image
; #:coloring is a function that will be applied to the match result and should return a color
; #:short-circuit means that the function stops as soon as the regex matches (default #t)
(define (regex-fractal 
         regex size 
         #:coloring [coloring (thunk* '#(1 1 1))] 
         #:short-circuit [short-circuit #t])
  (flomap->bitmap
   (build-flomap*
    3 size size
    (λ (x y)
      (let loop ([t 0] [l 0] [s size] [path ""])
        (cond
          ; If we're short circuiting, check the regex; if it matches, apply the coloring
          [(and short-circuit (regexp-match regex path)) => coloring]
          ; If we're at the last level, always apply the coloring function
          ; If it fails, always return black 
          [(<= s 1)
           (cond
             [(regexp-match regex path) => coloring]
             [else '#(0 0 0)])]
          ; Otherwise, divide the region into four subregions and recur into the proper one
          [else
           (define s/2 (quotient s 2))
           (define x-mid (+ l s/2))
           (define y-mid (+ t s/2))
           (loop 
            (if (< y y-mid) t y-mid)
            (if (< x x-mid) l x-mid)
            s/2
            (~a path 
                (match (list (< y y-mid) (< x x-mid))
                  ['(#t #t) 1]
                  ['(#t #f) 2]
                  ['(#f #t) 3]
                  ['(#f #f) 4])))]))))))

; ----- Examples -----

(define (demo size)
  (for/list ([f (in-list (list sierpinski 
                               sierpinski/length 
                               four-colors 
                               left/right 
                               jagged 
                               double-sierpinski 
                               ones-then-twos
                               ascending
                               no-one
                               even-sum
                               odd-sum
                               another-sierpinski
                               ones-at-the-end))])
    (list f (f size))))

; Calculate a sierpinski triangle by lighlighting anything with a 1
(define (sierpinski size) (regex-fractal #px"1" size))

; Do the same, but highlight each point by how far it took to get the match
(define (sierpinski/length size)
  (define path-length (size->path-length size))
  (regex-fractal
   #px".*1.*" size
   #:coloring
   (λ (m)
     (define l (string-length (car m)))
     (if (= l path-length)
         '#(1 1 1)
         (vector 0 (/ l path-length) 0)))))

; Calculate the longest sequence and map to different colors
; Disable short circuiting so that we get a full length match 
(define (four-colors size)
  (define path-length (size->path-length size))
  (regex-fractal
   #px"((.)(\\2*))" size
   #:short-circuit #f
   #:coloring
   (λ (m)
     (match m
       [#f '#(0 0 0)]
       [(list all _ which _)
        (define g (/ (string-length all) path-length))
        (case which
          [("1") (vector g g g)]
          [("2") (vector g 0 0)]
          [("3") (vector 0 g 0)]
          [("4") (vector 0 0 g)])]))))

; Split to either left or right pixels, matches either the first 1 or 2
(define (left/right size)
  (regex-fractal 
   #px"(1|2)" size
   #:coloring
   (λ (m)
     (match m
       [(list _ "1") '#(1 1 1)]
       [(list _ "2") '#(0 0 0)]))))

; Match a 1 followed immediately by a 2 to get a neat jagged shape
(define (jagged size) (regex-fractal #px"(12)" size))

; Match two sierpinski trianges (1 and 2)
(define (double-sierpinski size) (regex-fractal #px"(1.*2|2.*1)" size))

; Match patterns that have all 1s before all 2s (3s and 4s anywhere) 
; Interestingly, this ends up making a very similar shape to the double-sierpinski
(define (ones-then-twos size) (regex-fractal "^[34]*[134]*[34]*[234]*[34]*$" size #:short-circuit #f))

; Only images with strictly ascending points all the way down
(define (ascending size) (regex-fractal "^1*2*3*4*$" size #:short-circuit #f))

; Images without ones, of course this is the inverse of a sierpinski triangle
(define (no-one size) (regex-fractal "^[^1]*$" size #:short-circuit #f))

; Match regular expressions where the digits sum to an even/odd number
(define (even-sum size) (regex-fractal "^(2|4|[13][24]*[13])*$" size #:short-circuit #f))
(define (odd-sum size)  (regex-fractal "^(2|4|[13][24]*[13])*[13](2|4|[13][24]*[13])*$" size #:short-circuit #f))

; Another way to get the sierpinski triangle, anything that ends with ones
(define (another-sierpinski size) (regex-fractal "1$" size))

; Dots: anything that ends with ones, but this time don't circuit
(define (ones-at-the-end size) (regex-fractal "1$" size #:short-circuit #f))
