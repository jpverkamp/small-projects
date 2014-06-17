#lang racket

(require pict)

; Return a tree of the factors of n
(define (factor-tree n)
  (or 
   ; Try to find the first pair of factors
   ; Start from sqrt(n) and work down to get the largest factors first
   (for/first ([i (in-range (integer-sqrt n) 1 -1)]
               #:when (zero? (remainder n i)))
     ; Factor, create a tree with that node and it's further factors
     (list n
           (factor-tree i)
           (factor-tree (quotient n i))))
   ; If for/first returns #f there are no other factors, n is prime
   n))

; Render a tree structure
; Tree : (U (List Integer Tree Tree) Integer)
(define (render-factor-tree tr)
  (match tr
    ; Recursive tree, unpack the value and render subtrees
    [(list factor left right)
     (define v (text (~a factor)))
     (define l (render-factor-tree left))
     (define r (render-factor-tree right))
     ; Pin-line connects the nodes, append sets the trees side by side
     ; cb/ct-find tells the pins how to connect to the nodes (center bottom/top)
     (pin-line (pin-line (vc-append 10 v (ht-append 10 l r))
                         v cb-find
                         l ct-find)
               v cb-find
               r ct-find)]
    ; Values are directly rendered
    [prime
     (text (~a prime))]))
