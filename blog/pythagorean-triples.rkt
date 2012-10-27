#lang racket

(define (brute-count-triples-under sum)
  (define count 0)
  (for* ([b (in-range 1 sum)]
         [a (in-range 1 b)]
         #:when (let ([c (sqrt (+ (* a a) (* b b)))])
                  (and (<= (+ a b c) sum)
                       (integer? c))))
    (set! count (+ count 1)))
  count)

(require racket/generator)

(define (primitive-triples [max-sum #f])
  (generator ()
    (let loop ([a 3] [b 4] [c 5])
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
    (yield #f))) 

(define (primitive-triples-nogen [max-sum #f])
  (let loop ([a 3] [b 4] [c 5])
    (if (and max-sum (> (+ a b c) max-sum))
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
  
(define (triples-under sum)
  (for*/list ([trip (in-producer (primitive-triples sum) #f)]
              [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (map (lambda (n) (* n k)) trip)))

(define (triples-under-nogen sum)
  (for*/list ([trip (in-list (primitive-triples-nogen sum))]
              [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (map (lambda (n) (* n k)) trip)))
            
(define (count-triples-under sum)
  (define count 0)
  (for* ([trip (in-producer (primitive-triples sum) #f)]
         [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (set! count (+ count 1)))
  count)
  
(define (count-triples-under-nogen sum)
  (define count 0)
  (for* ([trip (in-list (primitive-triples-nogen sum))]
         [k (in-range 1 (+ 1 (quotient sum (apply + trip))))])
    (set! count (+ count 1)))
  count)
  