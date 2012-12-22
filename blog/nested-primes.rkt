#lang racket

; calculate how many digits there are in a number
(define (digits n)
  (let loop ([n n] [d 0])
    (if (= n 0)
        d
        (loop (quotient n 10) (+ d 1)))))

; test if m is evenly divisible by n
(define (divides? m n)
  (= 0 (remainder m n)))

; test if a number is prime
(define (prime? n)
  (cond
    [(< n 2) #f]
    [(= n 2) #t]
    [(divides? n 2) #f]
    [else
     (define root-n (+ 1 (integer-sqrt n)))
     (let loop ([i 3])
       (cond
         [(> i root-n) #t]
         [(divides? n i) #f]
         [else (loop (+ i 2))]))]))

; find the largest prime such that each number produced by
; removing digits from the left side is still prime
; #:root - the prime to start at, 0 for all primes
; #:limit - the largest prime to check
(define (largest-nested-prime 
         #:root [root 0] 
         #:limit [limit +inf.0])
  ; loop starting at the root
  (let loop ([n root])
    (cond
      ; if we're over the limit, no largest prime
      [(> n limit) 0]
      ; otherwise, try each prefixed digit while still prime
      [else
       (define multiplier (expt 10 (digits n)))
       ; use for/fold to emulate what for/max would do
       (for/fold ([best n])
                 ([i (in-range 1 10)]
                  #:when (prime? (+ (* multiplier i) n)))
         ; return the best of the current and the nested
         (max best (loop (+ (* multiplier i) n))))])))

; find a tree of primes
(define (nested-primes 
         #:root [root 0] 
         #:limit [limit +inf.0])
  (let loop ([n root])
    (cond
      ; special case to return all primes if n is 0
      [(= n 0)
       (for/list ([i (in-list '(2 3 5 7 9))]) (loop i))]
      ; stop recurring on non-primes or if the priems are too large
      [(or (not (prime? n))
           (> n limit))
       #f]
      ; otherwise build up the recursive lists
      [else
       (define multiplier (expt 10 (digits n)))
       ; all primes made from expanding the current n
       ; may be an empty list or nested list of lists
       (define recur
         (for*/list ([i (in-range 1 10)]
                     #:when (prime? (+ (* multiplier i) n))
                     [recur (in-list (list (loop (+ (* multiplier i) n))))]
                     #:when recur)
           recur))
       ; if we have a recursive solution, wrap it
       ; otherwise, just return it
       (if recur
           (cons n recur)
           (list n))])))
