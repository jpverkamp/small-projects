#lang racket

(require math/number-theory)

(define (prime-factors n)
  (map first (factorize n)))

(define (n-consecutive-with-m-factors n m)
  (let loop ([i 1] [count 0])
    (cond
      [(= count n) 
       (map (λ (n) (list n (factorize n)))
            (range (- i n) i))]
      [(= (length (prime-factors i)) m)
       (loop (+ i 1) (+ count 1))]
      [else 
       (loop (+ i 1) 0)])))

(define (n-consecutive-with-m-factors-sieved n m #:upper-bound [upper-bound 1000000])
  (define n-range (range n))
  (define sieve (make-vector upper-bound 0))
  (let loop ([i 2])
    ; Sieve but only on primes
    (when (zero? (vector-ref sieve i))
      (let loop ([j (+ i i)])
        (when (< j (vector-length sieve))
          (vector-set! sieve j (+ 1 (vector-ref sieve j)))
          (loop (+ j i)))))
    
    ; Check for consecutive primes
    (cond
      [(andmap (λ (Δ) (= (vector-ref sieve (+ i Δ)) m)) n-range)
       (map (λ (n) (list n (factorize n)))
            (range i (+ i n)))]
      [else
       (loop (+ i 1))])))

(define (n-consecutive-with-m-distinct-factors n m)
  (let loop ([i (+ n 2)]
             [factors (map (λ (Δ) (list->set (prime-factors (+ n 2 Δ)))) (range n))])
    (cond
      [(= m (set-count (apply set-union factors)))
       (map (λ (n) (list n (factorize n)))
            (range (- i n) i))]
      [else
       (loop (+ i 1)
             (append (rest factors)
                     (list (list->set (prime-factors (+ i 1))))))])))

(module+ test
  (require rackunit)
  
  (check-equal? 
   (n-consecutive-with-m-factors 2 2)
   '((14 ((2 1) (7 1))) 
     (15 ((3 1) (5 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors 3 3)
   '((644 ((2 2) (7 1) (23 1))) 
     (645 ((3 1) (5 1) (43 1))) 
     (646 ((2 1) (17 1) (19 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors 2 4)
   '((7314 ((2 1) (3 1) (23 1) (53 1))) 
     (7315 ((5 1) (7 1) (11 1) (19 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors 4 4)
   '((134043 ((3 1) (7 1) (13 1) (491 1)))
     (134044 ((2 2) (23 1) (31 1) (47 1)))
     (134045 ((5 1) (17 1) (19 1) (83 1)))
     (134046 ((2 1) (3 2) (11 1) (677 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors-sieved 2 2)
   '((14 ((2 1) (7 1))) 
     (15 ((3 1) (5 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors-sieved 3 3)
   '((644 ((2 2) (7 1) (23 1))) 
     (645 ((3 1) (5 1) (43 1))) 
     (646 ((2 1) (17 1) (19 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors-sieved 2 4)
   '((7314 ((2 1) (3 1) (23 1) (53 1))) 
     (7315 ((5 1) (7 1) (11 1) (19 1)))))
  
  (check-equal? 
   (n-consecutive-with-m-factors-sieved 4 4)
   '((134043 ((3 1) (7 1) (13 1) (491 1)))
     (134044 ((2 2) (23 1) (31 1) (47 1)))
     (134045 ((5 1) (17 1) (19 1) (83 1)))
     (134046 ((2 1) (3 2) (11 1) (677 1)))))
  
  (check-equal? (n-consecutive-with-m-distinct-factors 2 2)
                '((2 ((2 1))) (3 ((3 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 3)
                '((4 ((2 2))) (5 ((5 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 4)
                '((13 ((13 1))) (14 ((2 1) (7 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 5)
                '((64 ((2 6))) (65 ((5 1) (13 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 6)
                '((208 ((2 4) (13 1))) (209 ((11 1) (19 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 7)
                '((713 ((23 1) (31 1))) (714 ((2 1) (3 1) (7 1) (17 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 8)
                '((7313 ((71 1) (103 1))) (7314 ((2 1) (3 1) (23 1) (53 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 9)
                '((38569 ((38569 1))) (38570 ((2 1) (5 1) (7 1) (19 1) (29 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 2 10)
                '((254539 ((331 1) (769 1))) (254540 ((2 2) (5 1) (11 1) (13 1) (89 1)))))
  
  (check-equal? (n-consecutive-with-m-distinct-factors 3 3)
                '((3 ((3 1))) (4 ((2 2))) (5 ((5 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 4)
                '((2 ((2 1))) (3 ((3 1))) (4 ((2 2)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 5)
                '((12 ((2 2) (3 1))) (13 ((13 1))) (14 ((2 1) (7 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 6)
                '((32 ((2 5))) (33 ((3 1) (11 1))) (34 ((2 1) (17 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 7)
                '((152 ((2 3) (19 1))) (153 ((3 2) (17 1))) (154 ((2 1) (7 1) (11 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 8)
                '((284 ((2 2) (71 1))) (285 ((3 1) (5 1) (19 1))) (286 ((2 1) (11 1) (13 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 9)
                '((712 ((2 3) (89 1))) (713 ((23 1) (31 1))) (714 ((2 1) (3 1) (7 1) (17 1)))))
  (check-equal? (n-consecutive-with-m-distinct-factors 3 10)
                '((5794 ((2 1) (2897 1))) (5795 ((5 1) (19 1) (61 1))) (5796 ((2 2) (3 2) (7 1) (23 1)))))
  
  )