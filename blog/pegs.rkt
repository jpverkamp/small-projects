#lang racket

(require (prefix-in htdp: 2htdp/universe)
         (prefix-in htdp: 2htdp/image)
         graph)

; Puzzles are represented as a 15 element vector (#t for pegs)
; but can be entered as a 15 bit integer (1 for pegs)
(struct puzzle (data) #:transparent)
(define (make-puzzle v)
  (cond
    [(and (integer? v) (<= 0 v 32767))
     (puzzle (list->vector 
              (map (curry eq? #\1) 
                   (reverse (string->list (~a (number->string v 2) 
                                              #:width 15 
                                              #:align 'right 
                                              #:pad-string "0"))))))]
    [(and (vector? v) (= 15 (vector-length v)))
     (puzzle v)]
    [(and (list? v) (length v 15))
     (puzzle (list->vector v))]))

; Render a puzzle to text
(define (render-text puzzle)
  (for ([row (in-range 1 6)])
    (display (~a "" #:width (* 2 (- 6 row))))
    (for ([col (in-range 1 (+ 1 row))])
      (define i (+ (* 1/2 row (- row 1)) col))
      (display (~a (if (vector-ref (puzzle-data puzzle) (- i 1)) i "") #:width 4)))
    (newline)))

; Render a puzzle to a bitmap
(define (render puzzle)
  (define (bit-set? i) (vector-ref (puzzle-data puzzle) (- i 1)))
  
  (define imgs
    (for/list ([row (in-range 1 6)])
      (for/list ([col (in-range 1 (+ 1 row))])
        (define i (+ (* 1/2 row (- row 1)) col))
        (define color (if (bit-set? i) "black" "gray"))
        (htdp:overlay (htdp:text (~a i) 12 color)
                      (htdp:circle 10 "outline" color)
                      (htdp:circle 12 "solid" "white")))))
  
  (define rows (map (λ (row) (if (= 1 (length row)) 
                                 (first row)
                                 (apply htdp:beside row)))
                    imgs))
  
  (apply htdp:above rows))

; Count how many pegs are left in a puzzle
(define (count p)
  (vector-length (vector-filter identity (puzzle-data p))))

; Given a peg to move from and the peg to move over, return the new puzzle state
(define (jump p ifrom iover)
  (define from-list '(1  1  2  2  3  3  4  4  4  5  5  6  6  7  8  11 12 13))
  (define over-list '(2  3  4  5  5  6  5  7  8  8  9  9  10 8  9  12 13 14))
  (define to-list   '(4  6  7  9  8  10 6  11 13 12 14 13 15 9  10 13 14 15))
  
  (for/first ([from (in-list (append from-list to-list))]
              [over (in-list (append over-list over-list))]
              [to   (in-list (append to-list from-list))]
              #:when (and (= from ifrom)
                          (= over iover)
                          (vector-ref (puzzle-data p) (- from 1))
                          (vector-ref (puzzle-data p) (- over 1))
                          (not (vector-ref (puzzle-data p) (- to 1)))))
    (let ([new-data (vector-copy (puzzle-data p))])
      (vector-set! new-data (- from 1) #f)
      (vector-set! new-data (- over 1) #f)
      (vector-set! new-data (- to   1) #t)
      (puzzle new-data))))

; Get a list of all next states from a given puzzles
(define (next p)
  (filter identity
          (for*/list ([from (in-range 1 16)] 
                      #:when (vector-ref (puzzle-data p) (- from 1))
                      [over (in-range 1 16)] 
                      #:when (vector-ref (puzzle-data p) (- over 1)))
            (jump p from over))))

; Solve a puzzle using backtracking
(define (solve p)
  (cond
    [(= 1 (count p)) 
     (list p)]
    [else
     (let ([n (ormap solve (next p))])
       (and n (cons p n)))]))
