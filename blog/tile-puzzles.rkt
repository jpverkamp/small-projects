#lang racket

(require 2htdp/universe
         (prefix-in 2htdp/image: 2htdp/image)
         pict)

(define current-tile-size (make-parameter 50))

(define (char->color c)
  (case c
    [(#\R #\r) "red"]
    [(#\G #\g) "green"]
    [(#\B #\b) "blue"]
    [(#\C #\c) "cyan"]
    [(#\M #\m) "magenta"]
    [(#\Y #\y) "yellow"]
    [(#\K #\k) "black"]
    [else      "white"]))

(define (char->shape c)
  (case c
    [(#\R #\G #\B #\C #\M #\Y #\K) filled-rectangle]
    [(#\r #\g #\b #\c #\m #\y #\k) filled-ellipse]
    [(#\null)                      (λ _ (filled-rectangle 0 0))]))

; Render a single tile given a four character specifier
; Order is top, right, bottom, left
; Colors are cyan, magenta, yellow, red, green, blue, black (k for black)
; Uppercase are square, lowercase are circular
(define (render-tile tile)
  (match-define (list top right bottom left) (string->list tile))
  
  ; Size of the individual images
  (define quad-size (quotient (current-tile-size) 3))
  
  ; Offsets for pinning, zero/half/full size adjusted for quad size
  (define zs (- (quotient quad-size 2)))
  (define hs (- (quotient (current-tile-size) 2) (quotient quad-size 2)))
  (define fs (- (current-tile-size) (quotient quad-size 2)))
  
  ; Helper function to render a specific shape of the specific color
  (define (shape c) (colorize ((char->shape c) quad-size quad-size) (char->color c)))
  
  ; Construct the image by layering each of the four sides on the base
  (let* ([pict (rectangle (current-tile-size) (current-tile-size))]
         [pict (pin-under pict hs zs (shape top))]
         [pict (pin-under pict fs hs (shape right))]
         [pict (pin-under pict hs fs (shape bottom))]
         [pict [pin-under pict zs hs (shape left)]])
    (clip pict)))

; Render a puzzle of multiple tiles
; Puzzles are assumed to be square
(define (render puzzle)
  (define width (integer-sqrt (length puzzle)))
  
  (define tiles
    (for/list ([y (in-range width)])
      (for/list ([x (in-range width)])
        (render-tile (list-ref puzzle (+ x (* y width)))))))
  
  (define rows
    (map (λ (row) (apply (curry hc-append -1) row)) tiles))
  
  (apply (curry vc-append -1) rows))

; Score a puzzle, with 1 point per mismatched edge
; A score of 0 is solved
; An edge matches if the colors are the same and the shapes are different
(define (score puzzle)
  (define width (integer-sqrt (length puzzle)))
  
  ; If the letters are not the same, but the downcases are, they match
  ; If either charater is null, it counts as a match (edge cases)
  ; Saves the effort of matching each individual case
  (define (score c1 c2)
    (if (or (eq? c1 #\null)
            (eq? c2 #\null)
            (and (not (eq? c1 c2))
                 (eq? (char-downcase c1) (char-downcase c2))))
        0
        1))
  
  ; Get the tile at the specified coordinates within a puzzle
  (define (@ x y d)
    (if (or (< x 0) (>= x width)
            (< y 0) (>= y width))
        #\null
        (string-ref 
         (list-ref puzzle (+ x (* y width)))
         (case d [(t) 0] [(r) 1] [(b) 2] [(l) 3]))))
  
  ; Leave off a row/column to deal with edge cases
  (for*/sum ([y (in-range width)]
             [x (in-range width)])
    (+ (score (@ x y 'b) (@ x (+ y 1) 't))    ; Top/bottom edges
       (score (@ x y 'r) (@ (+ x 1) y 'l))))) ; Left/right edges
       

; Is this a currently valid solution to the puzzle
(define (valid? puzzle) (= 0 (score puzzle)))

; Insert an item into the given location in a list
(define (insert-at ls item x)
  (for/list ([i (in-naturals)]
             [el (in-list ls)])
    (if (= x i) item el)))

(define last-debug-steps '())

; Return a list of all rotated versions of a string
(define (rotations str)
  (for/list ([i (in-range (string-length str))])
    (string-append (substring str i) (substring str 0 i))))

; Solve a puzzle by ordering pieces so that they match
(define (solve puzzle #:debug [debug #f])
  (when debug (set! last-debug-steps '()))
               
  ; Start with an empty solution space (all null) and a list of pieces to place
  (let loop ([solution (make-list (length puzzle) "\0\0\0\0")]
             [to-place puzzle]
             [index    0])
    
    ; Record steps in debug mode so we can see the backtracking
    (when debug (set! last-debug-steps (cons solution last-debug-steps)))
    
    (cond
      ; If we've filled in all of the pieces, we have a solution
      [(= index (length puzzle))
       solution]
      ; Otherwise, try each piece, only recurring for those that fit
      ; Return the first that solves the puzzle from here, by recursion this will be a full solution
      [else
       (for*/first ([next-item (in-list to-place)]
                    [next-item-rotated (in-list (rotations next-item))]
                    [next-puzzle (in-value (insert-at solution next-item-rotated index))]
                    #:when (valid? next-puzzle)
                    [recur (in-value (loop next-puzzle (remove next-item to-place) (+ index 1)))]
                    #:when recur)
         recur)])))

; Use big-bang and debug mode to render a solution animation
(define (solve/render puzzle directory)
  (make-directory* directory)
  (time (solve (shuffle puzzle) #:debug #t))
  (define steps (reverse last-debug-steps))
  (for ([i (in-naturals)]
        [step (in-list steps)])
    (define path (build-path directory (~a "frame" (~a i #:width 4 #:align 'right #:pad-string "0") ".png")))
    (define img (render step))
    (displayln path)
    (send (pict->bitmap img) save-file path 'png)))

; Generate random puzzles
(define (random-puzzle size #:colors [colors 4])
  ; Generate n+1 intersections (including those off the edges)
  ; Each value is the top left corner of a tile with the right then down edge
  (define intersections
    (for/list ([y (in-range (+ size 1))])
      (for/list ([x (in-range (+ size 1))])
        (for/list ([which (in-list '(right down))])
          (list (string-ref "CMYKRGB" (random (min colors 7)))
                (if (= 0 (random 2)) 'normal 'inverse))))))
  
  (define (@ x y w invert?)
    (match-define (list char mode)
      (list-ref (list-ref (list-ref intersections y) x)
                (if (eq? w 'right) 0 1)))
    
    ((if (xor invert? (eq? mode 'inverse)) 
         char-downcase 
         identity)
     char))
  
  ; Fill out the tiles
  (shuffle
   (for*/list ([y (in-range size)]
               [x (in-range size)])
     (string (@ x       y       'right #f)
             (@ (+ x 1) y       'down  #f)
             (@ x       (+ y 1) 'right #t)
             (@ x       y       'down  #t)))))

(define (solve/annealing puzzle #:cooling [cooling 0.001])
  
  ; Randomly rotate a given percent of pieces
  (define (rotate puzzle chance)
    (for/list ([el (in-list puzzle)])
      (if (< (random) chance)
          (first (shuffle (rotations el)))
          el)))
  
  ; Randomly swap adjacent elements with given odds
  (define (swap puzzle chance)
    (let loop ([puzzle puzzle])
      (match puzzle
        [(list last) puzzle]
        [(list* a b rest)
         (if (< (random) chance)
             (list* b (loop (list* a rest)))
             (list* a (loop (list* b rest))))])))
  
  ; Perform simulated annealing across the solution space
  (let loop ([current-puzzle puzzle] [heat 1.0])
    (define current-score (score current-puzzle))
    (displayln (list heat current-score))
    
    (cond
      [(or (< heat 0.01) (= current-score 0))
       current-puzzle]
      [else
       (define next-puzzle (rotate (swap current-puzzle heat) heat))
       (define next-score (score next-puzzle))
       (loop (if (or (< next-score current-score)
                     (< (random) heat))
                 next-puzzle
                 current-puzzle)
             (* heat (- 1 cooling)))])))

(define (mod a b)
  (cond
    [(<  a 0) (mod (+ a b) b)]
    [(>= a b) (mod (- a b) b)]
    [else     a]))

(define (solve/genetic 
         puzzle
         #:population    [population-size 10] ; Number of genomes in each iteration
         #:cull-rate     [cull-rate 0.75]     ; Kill off the lowest this %
         #:mutation-rate [mutation-rate 0.01]  ; Mutate each new child at this %
         )
  
  (define keep-count (inexact->exact (floor (* population-size (- 1 cull-rate)))))
  
  ; A genome is a list of integers twice as long as the puzzle lenght
  ; Even indicies represent offsets into the list of still unused pieces
  ; Odd indicies represent rotation (start 0, each is 1/4 rotation cw)
    
  ; Generate a random genome of the correct length
  (define (random-genome)
    (apply append
           (for/list ([_ (in-list puzzle)])
             (list (random (length puzzle))     ; Offset
                   (random (length puzzle)))))) ; Rotation
  
  ; Convert a genome into a puzzle for scoring, etc purposes
  (define (genome->puzzle genome)
    (let loop ([pieces puzzle]  ; Pieces yet to place
               [puzzle '()]     ; The puzzle we are building
               [genome genome]) ; The genome (see above)
      (match genome
        [`() (reverse puzzle)]
        [`(,offset ,rotation . ,remaining-genome)
         (define piece   (list-ref pieces (mod offset (length pieces))))
         (define rotated (list-ref (rotations piece) (mod rotation 4)))
         (loop (remove piece pieces)
               (list* rotated puzzle)
               remaining-genome)])))
  
  ; Mutate a given genome by random adding ±1 to some elements
  (define (mutate genome)
    (for/list ([el (in-list genome)])
      (if (< (random) mutation-rate)
          (+ el (random 2) -1)
          el)))
  
  ; Breed two genomes by selecting a crossover point
  (define (breed mom dad)
    (define i (random (length mom)))
    (append (take mom i) (drop dad i)))
  
  (define initial-population 
    (for/list ([i (in-range population-size)])
      (random-genome)))
  
  (let loop ([population initial-population])
    ; Calculate the current fitness for each member
    ; Sort by fitness score
    (define population+fitness
      (sort
       (for/list ([el (in-list population)])
         (list (score (genome->puzzle el)) el))
       (λ (p1 p2) (< (first p1) (first p2)))))
    
    ; DEBUG
    (printf "min: ~a, avg: ~a\n"
            (first (first population+fitness))
            (/ (apply + (map first population+fitness))
               (length population+fitness)
               1.0))
    
    (cond
      ; If we have a zero fitness, we win!
      [(= 0 (first (first population+fitness)))
       (second (first population+fitness))]
      ; Otherwise, make the population better
      [else
       ; Remove the worst chunk of the population
       (define culled (map second (take population+fitness keep-count)))
       
       ; Breed some new members from what was left
       (define bred
         (append culled
                 (for/list ([i (in-range (- population-size keep-count))])
                   (breed (list-ref culled (random (length culled)))
                          (list-ref culled (random (length culled)))))))
       
       ; Mutate all of them (even the children)
       (define mutated (map mutate bred))
         
       ; Onwards and upwards!
       (loop mutated)])))
    
; ----- ----- ----- ----- ----- 
     
(define puzzle1
  (string-split
   "cymK
mCkY
ymkc
McMk
KYmC
KyCy
mkYc
MycK
cmKY" "\n"))

(define puzzle2 
  (string-split
   "cKCk
yYcc
YcCK
kKCM
CMKc
cKYC
kYcm
KYyY
Mccm
yKcm
mykK
MMCm
ckYC
ycmm
MmKM
kymc
KMMK
KcyM
kYck
YCKM
myYm
kYyY
CMKM
yYCM
YKyk" "\n"))

(define p '("cymK" "KyCy" "ymkc" "mkYc" "MycK" "mCkY" "cmKY" "KYmC" "McMk"))
     
#;(parameterize ([current-tile-size 50])
  (solve/render (shuffle puzzle2) "render"))
