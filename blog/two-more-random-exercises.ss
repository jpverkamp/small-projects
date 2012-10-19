; middle square PRNG: square the seed and take the middle digits
(define (make-middle-square n)
  (lambda ()
    (let ([digits (inexact->exact (ceiling (log n 10)))])
      (let ([res (mod (div (* n n) 
                        (expt 10 (/ digits 2))) (expt 10 digits))])
        (set! n res)
        res))]))))

; randu PRNG: x_n+1 = 65539 * x_n mod 2^31 (assume x_0 is odd)
(define make-randu
  (let ([2**31 (expt 2 31)])
    (lambda (x)
      (lambda ()
        (let ([res (mod (* 65539 x) 2**31)])
          (set! x res)
          res)))))