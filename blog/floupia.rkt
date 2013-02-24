#lang racket

; define a function that will accept payment from a list of coins
(define (make-coinage coins)
  ; generate all lists of n coins
  (define (make-coin-list n-coins)
    (cond
      [(= n-coins 0) '(())]
      [else
       (for*/list ([coin (in-list coins)]
                   [sublist (in-list (make-coin-list (- n-coins 1)))])
         (cons coin sublist))]))
  
  ; make a payment of a given value
  (define (make-payment value)
    (call/cc 
     (lambda (return)
       ; avoid staring contests
       (unless (zero? (remainder value (apply gcd coins)))
         (return #f))
       
       ; loop up through possible numbers of coins
       ; and each split within that of payment and change
       (for ([n-coins (in-naturals)])
         (for* ([coin-list (in-list (make-coin-list n-coins))]
                [split-at (in-range (+ 1 (length coin-list)))])
           ; if we have a valid payment, this is a minimal solution
           (define payment (take coin-list split-at))
           (define change (drop coin-list split-at))
           (when (= (- (apply + payment) value) (apply + change))
             (return (list payment change))))))))
  
  ; return the payment function
  make-payment)

; some sample currencies
(require rackunit)
(begin
  (define floupia (make-coinage '(1 3 7 31 153)))
  (check-equal? (floupia 17) '((3 7 7) ()))
  (check-equal? (floupia 99) '((1 7 153) (31 31)))
  (check-equal?  (floupia 57) '((1 1 31 31) (7)))

  (define us (make-coinage '(1 5 10 25 50 100)))
  (define uk (make-coinage '(1 2 5 10 20 50 100 200)))
  (check-equal?  (us 119) '((10 10 100) (1)))
  (check-equal?  (us 123) '((25 100) (1 1)))
  (check-equal?  (uk 119) '((20 100) (1)))
  (check-equal?  (uk 123) '((1 2 20 100) ()))

  (define harry-potter (make-coinage '(1 17 493)))
  (check-equal? (harry-potter 100) '((17 17 17 17 17 17) (1 1)))

  (define oddness (make-coinage '(3 6)))
  (check-equal? (oddness 12) '((6 6) ()))
  (check-false (oddness 11)))
