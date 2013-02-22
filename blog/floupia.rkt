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
       ; loop up through possible numbers of coins
       ; and each split within that of payment and change
       (for ([n-coins (in-naturals)])
         (for* ([coin-list (in-list (make-coin-list n-coins))]
                [split-at (in-range (length coin-list))])
           ; if we have a valid payment, this is a minimal solution
           (define payment (take coin-list split-at))
           (define change (drop coin-list split-at))
           (when (= (- (apply + payment) value) (apply + change))
             (return (list payment change))))))))
  
  ; return the payment function
  make-payment)

; some sample currencies
(define floupia (make-coinage '(1 3 7 31 153)))
(define us (make-coinage '(1 5 10 25 50 100)))
(define uk (make-coinage '(1 2 5 10 20 50 100 200)))
(define harry-potter (make-coinage '(1 17 493)))