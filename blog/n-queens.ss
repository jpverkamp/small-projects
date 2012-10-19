; solve the n-queens problem
(define (queens n)
  (let loop ([queens '()] [row 0] [col 0])
    (cond 
      ; if we get to the last row, we're done
      [(= row n) (list queens)]
      ; at the end of a column, we can't place any queens
      [(= col n) '()]
      [else
       ; place a new queen
       (let ([new-queen (list row col)])
         ; if any queen can attack us:
         (if (any? (lambda (old-queen)
                     (or (= (car old-queen) (car new-queen))
                         (= (cadr old-queen) (cadr new-queen))
                         (= 1 (abs (/ (- (cadr old-queen) (cadr new-queen)) 
                                      (- (car old-queen) (car new-queen)))))))
               queens)
             ; don't use this square
             (loop queens row (+ col 1))
             ; try both, use and don't use this square
             (append 
               (loop (cons new-queen queens) (+ row 1) 0)
               (loop queens row (+ col 1)))))])))

; does any item in l match the predicate ?
(define (any? ? l) (not (null? (filter ? l))))