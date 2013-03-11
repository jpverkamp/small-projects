#lang racket

; represent a square on the board
(define-struct point (x y) #:transparent)

; test if a square is still on the board
; use 0 based indexing
(define (in-bounds? pt)
  (and (>= (point-x pt) 0) (< (point-x pt) 8)
       (>= (point-y pt) 0) (< (point-y pt) 8)))

; return the eight neighboring points for a knight
; a for loop without for* will loop in sync across the lists
(define (knight-neighbors pt)
  (for/list ([xd (in-list '(-2 -2 -1 -1  1  1  2  2))]
             [yd (in-list '(-1  1 -2  2 -2  2 -1  1))])
    (make-point (+ (point-x pt) xd)
                (+ (point-y pt) yd))))

; find all paths from src to dst in exactly the given moves
(define (knight-moves src dst moves)
  (cond
    ; if we're out of moves, check if we've found a solution
    ; either return just the destination or the empty list
    [(= moves 0)
     (if (equal? src dst) (list dst) '())]
    ; otherwise, step in all 8 possible directions
    ; only keep solutions that stay on the board
    [else
     (for/list ([nxt (in-list (knight-neighbors src))]
                #:when (in-bounds? nxt) 
                [recur (in-list (knight-moves nxt dst (- moves 1)))])
       (cons src recur))]))