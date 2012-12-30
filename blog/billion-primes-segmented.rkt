#lang racket

(provide sum-primes)

(define div quotient)
(define mod remainder)

; test if n divides m
(define (divides? m n)
  (= 0 (remainder m n)))

; estimate the nth prime, return lower and upper bounds
; source: http://en.wikipedia.org/wiki/Prime_number_theorem
(define (guess-nth-prime n)
  (values (inexact->exact 
           (floor (* n (log n))))
          (inexact->exact 
           (ceiling
            (if (<= n 1) 
                3 
                (+ (* n (log n)) (* n (log (log n)))))))))

; sum the first n primes using a segmented sieve of eratosthenes
(define (sum-primes n)
  ; sieve up to sqrt of the estimated high prime
  (define-values (lo hi) (guess-nth-prime n))
  (define sieve-size (+ 1 (integer-sqrt hi)))
  (define sieve (make-vector sieve-size #t))
  (vector-set! sieve 0 #f)
  (vector-set! sieve 1 #f)
  (for* ([i (in-range 2 sieve-size)]
         #:when (vector-ref sieve i)
         [j (in-range (* i i) sieve-size i)])
    (vector-set! sieve j #f))
  
  ; create a list of just those primes
  (define primes
    (for/list ([i (in-naturals)]
               [p? (in-vector sieve)]
               #:when p?)
      i))
  
  ; reuse the sieve in blocks until we've found enough
  (define sum (apply + primes))
  (define cnt (length primes)) 
  (let loop ([lo sieve-size]) ; start of current block
    ; clear the vector
    (for ([i (in-range sieve-size)]) (vector-set! sieve i #t))
    
    ; remove all multiples of candidate primes
    (for* ([p (in-list primes)]
           [i (in-range (if (divides? lo p) 0 (- p (mod lo p))) sieve-size p)])
      (vector-set! sieve i #f))
    
    ; add new primes to sum and count
    (for ([i (in-range lo (+ lo sieve-size))]
          [p? (in-vector sieve)]
          #:when (and p? (< cnt n)))
      (set! sum (+ sum i))
      (set! cnt (+ cnt 1)))
    
    ; if we're done, return the result, otherwise loop
    (if (= cnt n)
        sum
        (loop (+ lo sieve-size)))))

(for ([nstr (in-vector (current-command-line-arguments))])
  (define n (string->number nstr))
  (printf "~s:\n" n)
  (printf "~s\n\n" (time (sum-primes n))))
		 