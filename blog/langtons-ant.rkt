#lang racket

(require (only-in 2htdp/image rectangle beside above empty-scene overlay isosceles-triangle rotate)
         (only-in 2htdp/universe big-bang on-tick to-draw record?)
         (only-in racket/draw the-color-database)
         (only-in srfi/1 reduce))

(struct ant (rule location direction grid) #:transparent)
(define (make-ant rule) (ant rule 0 0+i (hash)))

; Update an ant
(define (tick a)
  ; Unpack the previous ant, get the current cell
  (match-define (ant rule location direction grid) a)
  (define cell (hash-ref grid location 0))
  
  ; Rotate, multiply by e^iθ, which for 90° left or right is ±i
  (define new-direction (* direction (if (eq? #\L (string-ref rule cell)) 0+i 0-i)))
  
  ; Create and return the new ant
  ; Update the position via direction and move to the next state (wrapping)
  (ant rule
       (+ location new-direction)
       new-direction
       (hash-set grid location (remainder (+ cell 1) (string-length rule)))))

; Run multiple ticks sequentially
(define (fast-tick a n)
  (for/fold ([a a]) ([i (in-range n)])
    (tick a)))

; Return the current bounds for an ant
(define (bounds a)
  (for/fold ([min-x +inf.0] [max-x -inf.0] [min-y +inf.0] [max-y -inf.0])
            ([(index cell) (in-hash (ant-grid a))])
    (values (min min-x (real-part index))
            (max max-x (real-part index))
            (min min-y (imag-part index))
            (max max-y (imag-part index)))))

; Render an ant into ASCII characters
(define (render/ascii a [charset " .:-=+*#%@"])
  ; Unpack the ant and determine how large of a grid we need
  (match-define (ant rule location direction grid) a)
  (define-values (min-x max-x min-y max-y) (bounds a))
  
  ; Sanity check the given charset
  (when (> (string-length rule) (string-length charset))
    (error 'render-ascii "Charset is not longer enough, need ~a, given ~a" (string-length rule) (string-length charset)))
  
  ; Render an ASCII grid to current-output-port
  ; inexact->exact is necessary to avoid floating point hash errors
  (for ([y (in-range min-y (+ max-y 1))])
    (for ([x (in-range min-x (+ max-x 1))])
      (define p (inexact->exact (make-rectangular x y))) 
      (display (string-ref charset (hash-ref grid p 0))))
    (newline)))

; Render using htdp
(define (render/2htdp a [colors '#("white" "black" "red" "blue" "green" "yellow" "magenta" "cyan" "gray" "pink")])
  ; Unpack the ant and determine how large of a grid we need
  (match-define (ant rule location direction grid) a)
  (define-values (min-x max-x min-y max-y) (bounds a))
  
  ; Sanity check that we have enough colors, then generate some
  (when (> (string-length rule) (vector-length colors))
    (error 'render-ascii "Not enough colors, need ~a, given ~a" (string-length rule) (vector-length colors)))

  ; Generate the raw images
  (define images
    (for/list ([y (in-range   (- min-y 1) (+ max-y 2))])
      (for/list ([x (in-range (- min-x 1) (+ max-x 2))])
        (define p (inexact->exact (make-rectangular x y))) 
        (define c (vector-ref colors (hash-ref grid p 0)))
        (define block (rectangle 10 10 "solid" c))
        (if (= p location)
            (rotate (case direction [(0+i) 0] [(1) 90] [(0-i) 180] [(-1) 270])
                    (overlay (isosceles-triangle 5 45 'outline "red")
                             (isosceles-triangle 5 45 'solid "black")
                             block))
            block))))

  ; Combine them
  (define null (empty-scene 0 0))
  (foldl above null (map (λ (row) (foldl beside null row)) images)))

; Simulate a rule using big bang
(define (simulate rule [width 800] [height 600])
  (define background (empty-scene width height))
  (big-bang (make-ant rule)
    [on-tick tick]
    [to-draw (λ (ant) (overlay (render/2htdp ant) background))]
    [record? #t]))

; Generate a random rule of the given length
(define (random-rule [length 10])
  (list->string 
   (for/list ([i (in-range length)]) 
     (if (zero? (random 2)) #\L #\R))))
