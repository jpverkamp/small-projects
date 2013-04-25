#lang typed/racket

(provide sum-primes)

(define div quotient)
(define mod remainder)

; test if n divides m
(: divides? (Integer Integer -> Boolean))
(define (divides? m n)
  (= 0 (remainder m n)))

; estimate the nth prime, return lower and upper bounds
; source: http://en.wikipedia.org/wiki/Prime_number_theorem
(: guess-nth-prime (Integer -> (Values Integer Integer)))
(define (guess-nth-prime n)
  (values 
   (exact-floor (* n (real-part (log n))))
   (exact-ceiling
    (if (<= n 1) 
        3 
        (+ (* n (real-part (log n))) 
           (* n (real-part (log (log n)))))))))

; sum the first n primes using a segmented sieve of eratosthenes
(: sum-primes (Integer -> Integer))
(define (sum-primes n)
  ; sieve up to sqrt of the estimated high prime
  (: lo Integer) (: hi Integer) (: sieve-size Integer)
  (define-values (lo hi) (guess-nth-prime n))
  (define sieve-size 
    (let: ([temp : Number (sqrt hi)])
      (exact-ceiling n)))
  
  (: sieve (Vectorof Boolean))
  (define sieve (make-vector sieve-size #t))
  (vector-set! sieve 0 #f)
  (vector-set! sieve 1 #f)
  
  (for*: ([i : Integer (in-range 2 sieve-size)]
          #:when (vector-ref sieve i)
          [j : Integer (in-range (* i i) sieve-size i)])
    (vector-set! sieve j #f))
  
  ; create a list of just those primes
  (define: primes : (Listof Integer)
    (for/list: ([i : Integer (in-naturals)]
                [p? : Boolean (in-vector sieve)]
                #:when p?)
      i))
  
  ; reuse the sieve in blocks until we've found enough
  (: sum Integer) (: cnt Integer)
  (define sum (apply + primes))
  (define cnt (length primes)) 
  
  (: lo Integer)
  (let loop ([lo sieve-size]) ; start of current block
    ; clear the vector
    (for: ([i : Integer (in-range sieve-size)]) 
      (vector-set! sieve i #t))
    
    ; remove all multiples of candidate primes
    (for*: ([p : Integer (in-list primes)]
            [i : Integer (in-range (if (divides? lo p) 0 (- p (mod lo p))) sieve-size p)])
      (vector-set! sieve i #f))
    
    ; add new primes to sum and count
    (for: ([i : Integer (in-range lo (+ lo sieve-size))]
           [p? : Boolean (in-vector sieve)]
           #:when (and p? (< cnt n)))
      (set! sum (+ sum i))
      (set! cnt (+ cnt 1)))
    
    ; if we're done, return the result, otherwise loop
    (if (= cnt n)
        sum
        (loop (+ lo sieve-size)))))

(for: ([nstr : String (in-vector (current-command-line-arguments))])
  (cond
    [(string->number nstr)
     => (lambda (n)
          (cond 
            [(integer? n)
             (printf "~s:\n" n)
             (printf "~s\n\n" (time (sum-primes (exact-round n))))]
            [else
             (printf "~s is not an integer\n" n)]))]
    [else
     (printf "~s is not a number\n" nstr)]))
		 