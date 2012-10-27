#lang racket

; ----- ORIGINAL BRUTE FORCE VERSION -----

; calculate the number of pythagorean triples with perimeters under a given value directly
(define (brute-count-triples-under sum)
  (define count 0)
  (for* ([b (in-range 1 sum)]
         [a (in-range 1 b)]
         #:when (let ([c (sqrt (+ (* a a) (* b b)))])
                  (and (<= (+ a b c) sum)
                       (integer? c))))
    (set! count (+ count 1)))
  count)

; ----- FIRST OPTIMIZED VERSION, HALL'S + GENERATORS -----
(require racket/generator)

; return a generator of all primitive pythagorean triples (uses Hall's algorithm)
(define (primitive-triples [max-sum #f])
  (generator ()
    ; start with the smallest triple: 3, 4, 5
    (let loop ([a 3] [b 4] [c 5])
      ; break brances that go beyond the maximum sum
      ; this is a depth first search, so if you don't specifiy a max sum,
      ; you'll never get the other two branches
      (when (or (not max-sum) (<= (+ a b c) max-sum))
        (yield (list (min a b) (max a b) c))
        ; a – 2b + 2c, 2a – b + 2c, 2a – 2b + 3c
        (loop (+ a (* -2 b) (* 2 c))
              (+ (* 2 a) (- b) (* 2 c))
              (+ (* 2 a) (* -2 b) (* 3 c)))
        ; a + 2b + 2c, 2a + b + 2c, 2a + 2b + 3c
        (loop (+ a (* 2 b) (* 2 c))
              (+ (* 2 a) b (* 2 c))
              (+ (* 2 a) (* 2 b) (* 3 c)))
        ; -a + 2b + 2c, -2a + b + 2c, -2a + 2b + 3c
        (loop (+ (- a) (* 2 b) (* 2 c))
              (+ (* -2 a) b (* 2 c))
              (+ (* -2 a) (* 2 b) (* 3 c)))))
    ; yield #f to signify that we're done
    (yield #f))) 

; use the above function to return a list of all triples under a sum
(define (triples-under sum)
  (for*/list ([trip (in-producer (primitive-triples sum) #f)]
              [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (map (lambda (n) (* n k)) trip)))

; directly count the triples rather than finding them then using length
(define (count-triples-under sum)
  (define count 0)
  (for* ([trip (in-producer (primitive-triples sum) #f)]
         [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (set! count (+ count 1)))
  count)

; ----- UPDATED VERSION THAT RETURNS THE LIST W/O A GENERATOR ----

; return a generator of all primitive pythagorean triples (uses Hall's algorithm)
; max-sum is not optional as we cannot return an infinite list
; (or rather don't want to)
(define (primitive-triples-nogen max-sum)
  (let loop ([a 3] [b 4] [c 5])
    (if (> (+ a b c) max-sum)
        '()
        (append
         (list (list (min a b) (max a b) c))
         ; a – 2b + 2c, 2a – b + 2c, 2a – 2b + 3c
         (loop (+ a (* -2 b) (* 2 c))
               (+ (* 2 a) (- b) (* 2 c))
               (+ (* 2 a) (* -2 b) (* 3 c)))
         ; a + 2b + 2c, 2a + b + 2c, 2a + 2b + 3c
         (loop (+ a (* 2 b) (* 2 c))
               (+ (* 2 a) b (* 2 c))
               (+ (* 2 a) (* 2 b) (* 3 c)))
         ; -a + 2b + 2c, -2a + b + 2c, -2a + 2b + 3c
         (loop (+ (- a) (* 2 b) (* 2 c))
               (+ (* -2 a) b (* 2 c))
               (+ (* -2 a) (* 2 b) (* 3 c)))))))
  
(define (triples-under-nogen sum)
  (for*/list ([trip (in-list (primitive-triples-nogen sum))]
              [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (map (lambda (n) (* n k)) trip)))

(define (count-triples-under-nogen sum)
  (define count 0)
  (for* ([trip (in-list (primitive-triples-nogen sum))]
         [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (set! count (+ count 1)))
  count)

; ----- COMPROMISE VERSION THAT MAKES A HOME GROWN GENERATOR -----

; return a psuedo-generator of all primitive pythagorean triples (uses Hall's algorithm)
(define (primitive-triples-state [max-sum #f])
  ; initialize the state with the smallest triple: 3, 4, 5
  (define triples '((3 4 5)))
  ; repeatedly call to return the next triple
  (lambda ()
    ; loop until we find one under max-sum
    (let loop ()
      (cond
        ; we've found all of the triples
        [(null? triples) #f]
        [else
         (define a (caar triples))
         (define b (cadar triples))
         (define c (caddar triples))
         (cond
           ; this branch is too large, try the next one
           [(and max-sum (> (+ a b c) max-sum))
            (set! triples (cdr triples))
            (loop)]
           ; otherwise, add the three from this branch and return
           [else
            (define r (car triples))
            (set! triples
                  (list*
                   ; a – 2b + 2c, 2a – b + 2c, 2a – 2b + 3c
                   (list (+ a (* -2 b) (* 2 c))
                         (+ (* 2 a) (- b) (* 2 c))
                         (+ (* 2 a) (* -2 b) (* 3 c)))
                   ; a + 2b + 2c, 2a + b + 2c, 2a + 2b + 3c
                   (list (+ a (* 2 b) (* 2 c))
                         (+ (* 2 a) b (* 2 c))
                         (+ (* 2 a) (* 2 b) (* 3 c)))
                   ; -a + 2b + 2c, -2a + b + 2c, -2a + 2b + 3c
                   (list (+ (- a) (* 2 b) (* 2 c))
                         (+ (* -2 a) b (* 2 c))
                         (+ (* -2 a) (* 2 b) (* 3 c)))
                   ; all of the rest
                   (cdr triples)))
            r])]))))  

; count all of the triples under the given sum
(define (count-triples-under-state sum)
  (define count 0)
  (for* ([trip (in-producer (primitive-triples-state sum) #f)]
         [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (set! count (+ count 1)))
  count)