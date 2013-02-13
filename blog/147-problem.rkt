#lang racket

; find sum(1/xi, i = 1 .. k) = 1
; for example, with k = 5:
; 1/2 + 1/4 + 1/7 + 1/14 + 1/28 = 1
(define (147-puzzle k)
  (let loop ([depth k]       ; the number of fractions left to add
             [lower-bound 2] ; the lower bound on new demoninator
             [target 1]      ; the remaining target to add up to
             [so-far '()])   ; the fractions so far
    (cond
      ; one more fraction and what's left has numerator 1, so valid
      [(and (= depth 1) (= 1 (numerator target)))
       (list (cons target so-far))]
      ; one more but not a valid last fraction
      [(= depth 1)
       '()]
      ; otherwise, recur
      ; range is bounded by the lower bound and the current fraction
      ; nested for*/list automatically appends into a larger list
      [else
       (for*/list ([i (in-range (max lower-bound (+ 1 (floor (/ 1 target))))
                                (+ 1 (floor (* depth (/ 1 target)))))]
                   [res (loop (- depth 1) i (- target (/ 1 i)) (cons (/ 1 i) so-far))])
         res)])))