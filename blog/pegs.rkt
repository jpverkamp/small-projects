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

; Get a numerican index for a puzzle
(define (index p)
  (string->number 
   (list->string 
    (for/list ([v (in-vector (puzzle-data p))]) (if v #\1 #\0)))
   2))

; Rotate a puzzle clockwise
(define (rotate p)
  (puzzle (for/vector ([i (in-list '(11 12 7 13 8 4 14 9 5 2 15 10 6 3 1))])
            (vector-ref (puzzle-data p) (- i 1)))))

; Reflect a puzzle left to right
(define (reflect p)
  (puzzle (for/vector ([i (in-list '(1 3 2 6 5 4 10 9 8 7 15 14 13 12 11))])
            (vector-ref (puzzle-data p) (- i 1)))))

; Invert a puzzle (mostly for creating puzzles)
(define (invert p)
  (puzzle (for/vector ([v (in-vector (puzzle-data p))]) (not v))))

; Minimize a puzzle by finding the reflection/rotation with the minimal vector
(define (minify p)
  (define r1 (rotate p))
  (define r2 (rotate r1))
  (first (sort (list p r1 r2 (reflect p) (reflect r1) (reflect r2))
               (λ (p1 p2)
                 (< (index p1) (index p2))))))

; Count how many total states are reachable from any initial state
; By default, start with one copy of each peg missing
(define (reachable [queue (for/list ([i (in-range 15)])
                            (invert (make-puzzle (expt 2 i))))])
  (let loop ([reached (hash)] [queue queue])
    (cond
      ; Queue is empty, done
      [(null? queue)
       reached]
      ; Already checked this state, check the rest
      [(hash-ref reached (index (first queue)) #f)
       (loop reached (rest queue))]
      ; New state, add it to the hash and all next states to the queue
      [else
       (loop (hash-set reached (index (first queue)) #t)
             (append (rest queue) (next (first queue))))])))

; Modification of reachable states, only minified 
; By default, start with one copy of each peg missing
(define (reachable-min [queue (for/list ([i (in-range 15)])
                                (invert (make-puzzle (expt 2 i))))])
  (let loop ([reached (hash)] [queue queue])
    (cond
      ; Queue is empty, done
      [(null? queue)
       reached]
      ; Already checked this state, check the rest
      [else
       (define p (minify (first queue)))
       (define i (index p))
       (cond
         [(hash-ref reached i #f) 
          (loop reached (rest queue))]
         [else
          (loop (hash-set reached i #t)
                (append (rest queue) (next p)))])])))

; Find a map of all possible moves from a given puzzle
(define (all-moves p)
  (define moves (make-hash))
  (let loop ([p p])
    (let ([p (minify p)])
      (define i (index (minify p)))
      (when (not (hash-has-key? moves i))
        (define next-ps (next p))
        (hash-set! moves i (list->set (map index (map minify next-ps))))
        (for-each loop next-ps))))
  moves)

; Count the number of winning and losing states from a given puzzle
(define (score p)
  (define moves (all-moves p))
  (define-values (wins losses)
    (let loop ([i (index (minify p))])
      (define nxt (hash-ref moves i (set)))
      (cond
        [(set-empty? nxt)
         (if (= 1 (count (make-puzzle i)))
             (values 1 0)
             (values 0 1))]
        [else
         (for/fold ([wins 0] [losses 0]) ([n (in-set nxt)])
           (define-values (r-wins r-losses) (loop n))
           (values (+ wins   r-wins)
                   (+ losses r-losses)))])))
  (* 1.0 (/ wins (+ wins losses))))

; Make a graph of all moves for a given puzzle
(define (graph-all-moves p)
  (define moves (all-moves p))
  (define colors 
    (for/hash ([(k _) (in-hash moves)])
      (values k (cond
                  [(= k (index p)) 0] ; Start state
                  [(= 1 (count (make-puzzle k))) 1] ; End state
                  [else 2])))) ; Everything else

  (graphviz #:colors colors
            (unweighted-graph/directed 
             (for*/list ([(k v*) (in-hash moves)]
                         [v      (in-set v*)])
               (list k v)))))


(define (play p)
  (cond
    [(= 1 (count p))         (displayln "YOU WIN!")]
    [(= 0 (length (next p))) (display "You lose. :(")]
    [else
     (render-text p)
     (displayln "Enter the peg to use and the peg to jump")
     (define from (read))
     (define over (read))
     (cond
       [(jump p from over) => play]
       [else 
        (displayln "Invalid move.")
        (play p)])]))
