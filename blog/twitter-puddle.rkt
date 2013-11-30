#lang racket

(require
 2htdp/image
 2htdp/universe)

; Twitter puddle structure; 2d grid of item types (empty, wall, water)
(struct puddle (data) #:transparent #:mutable)

(define current-block-size (make-parameter 10))

; Convert a puddle into a pict
(define (draw-puddle p)
  (define (make-row row)
    (apply beside
           (for/list ([col (in-vector row)])
             (rectangle
              (current-block-size) (current-block-size)
              "solid"
              (case col
                [(empty) "white"]
                [(wall)  "black"]
                [(water) "blue"])))))
  
  (apply above
         (reverse 
          (for/list ([row (in-vector (puddle-data p))])
            (make-row row)))))

; Create a puddle from a list of wall heights
(define (make-puddle wall-heights)
  (define w (length wall-heights))
  (define h (add1 (apply max wall-heights)))
  
  (puddle
   (for/vector ([r (in-range h)])
     (for/vector ([c (in-range w)]
                  [h (in-list wall-heights)])
       (if (< r h) 'wall 'empty)))))

; Getter/setter for puddle data
(define (puddle-ref p r c)
  (with-handlers ([exn? (λ (e) #f)])
    (vector-ref (vector-ref (puddle-data p) r) c)))

(define (puddle-set!? p r c v)
  (with-handlers ([exn? (λ (e) #f)])
    (vector-set! (vector-ref (puddle-data p) r) c v))
  #t)

; Get the size of a puddle
(define (puddle-height p)
  (vector-length (puddle-data p)))

(define (puddle-width p)
  (vector-length (vector-ref (puddle-data p) 0)))

; Perform one step in puddle simulation
; Try moving each water these directions in order: 
; - down, down left, down right, left, right, no move (will always succeed)
(define (step p)
  ; Create the new puddle (if you do it in place, water teleports)
  (define new-p
    (puddle (for/vector ([row (in-vector (puddle-data p))])
              (for/vector ([col (in-vector row)])
                (case col
                  [(empty water) 'empty]
                  [(wall)        'wall])))))
  
  ; Update each element in turn
  (for ([r (in-naturals)]
        [row (in-vector (puddle-data p))])
    (for ([c (in-naturals)]
          [col (in-vector row)])
      (let/ec break
        (when (eq? 'water col)
          (for ([rd (in-list '(-1 -1 -1  0  0  0  1))]
                [cd (in-list '( 0 -1  1 -1  1  0  0))])
            (case (puddle-ref new-p (+ r rd) (+ c cd))
              [(empty #f)
               (when (puddle-set!? new-p (+ r rd) (+ c cd) 'water)
                 (break))]))))))
  
  new-p)

; Add water to the top row (randomly if column is #f)
(define (drip p [column #f])
  ; Copy the old puddle
  (define new-p
    (puddle (for/vector ([row (in-vector (puddle-data p))])
              (for/vector ([col (in-vector row)])
                col))))
  
  ; Insert at a specified location or randomly
  (define r (sub1 (puddle-height p)))
  (cond
    [column 
     (puddle-set!? new-p r column 'water)]
    [else
     (let/ec break
       (for ([c (in-list (shuffle (range (puddle-width p))))])
         (when (puddle-set!? new-p r c 'water)
           (break))))])
  
  new-p)

; Count how much of any given thing is in a world
(define (puddle-count puddle thing)
  (for*/sum ([r (in-range (puddle-height puddle))]
             [c (in-range (puddle-width puddle))]
             #:when (eq? thing (puddle-ref puddle r c)))
    1))

; Run an entire simulation until steady state
(define (simulate puddle)
  (define previous-states          (make-hash))
  (define (previous-state? puddle) (hash-has-key? previous-states puddle))
  (define (add-state! puddle)      (hash-set! previous-states puddle #t))

  (define potential-drips (shuffle (range (puddle-width puddle))))
  (define stop? #f)
  
  (big-bang puddle
    [record? "output"]
    [stop-when (λ (puddle) stop?)]
    [on-draw draw-puddle]
    [on-tick 
     (λ (puddle)
       (let/ec return
         ; We've not seen the stepped case before, use that
         (define stepped (step puddle))
         (when (and (not (equal? stepped puddle))
                    (not (previous-state? stepped)))
           (add-state! stepped)
           (return stepped))
         
         ; If we don't have any potential drips left, we've reached steady state
         ; Try inverting first
         (when (null? potential-drips)
           (when (equal? stepped puddle) 
             (printf "steady state: ~a\n" (puddle-count puddle 'water))
             (set! stop? #t))
           
           (return stepped))
         
         ; Try to drip instead, if we've seen this drip, don't drip here again
         (define dripped (drip puddle (car potential-drips)))
         (when (or (equal? dripped puddle)
                   (previous-state? dripped))
           (set! potential-drips (cdr potential-drips)))
         
         (return dripped)))]))

; Make a random level with the maximum width and height
(define (random-puddle width max-height)
  (make-puddle
   (for/list ([r (in-range width)])
     (random max-height))))
