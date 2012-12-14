#lang racket

(define div quotient)
(define mod remainder)

; get the digits of a number
(define (digits-of n)
  (cond
    [(= n 0) '(0)]
    [else
     (let loop ([n n] [ans '()])
       (cond
         [(= n 0) ans]
         [else
          (loop (div n 10) (cons (mod n 10) ans))]))]))

; compare two lists, ignoring ordering
(define (unordered-equal? ls1 ls2)
  (equal? (sort ls1 <)
          (sort ls2 <)))

; return only unique results
(define (unique ls)
  (cond
    [(null? ls) ls]
    [else
     (cons (car ls)
           (unique (filter 
                    (lambda (n) (not (= n (car ls)))) 
                    (cdr ls))))]))

; find all narcissistic numbers of length n
; narcissistic numbers are those which have the following property
; i = sum(d^n) for digit d in number i of length n
(define (old-narcissistic n)
  ; exponents
  (define expts (for/vector ([i (in-range 10)]) (expt i n)))
  
  ; upper bound on n
  (define bound (expt 10 n))
  
  ; find all numbers
  (sort
   (unique
    ; digits - digits added so far
    ; sum - the current sum of powers
    (let loop ([digits '()] [sum 0])
      (cond 
        ; if we have enough digits, check for a valid solution
        [(= n (length digits))
         (if (unordered-equal? digits (digits-of sum))
             (list sum)
             '())]
        
        ; if the sum is too large, bail out
        [(>= sum bound)
         '()]
        
        ; otherwise, recur on all possible digits
        [else
         (for*/list ([i (in-range 10)]
                     [res (in-list (loop (cons i digits) 
                                         (+ (vector-ref expts i) sum)))])
           res)])))
   <))
  
; find all narcissistic numbers of length n
; narcissistic numbers are those which have the following property
; i = sum(d^n) for digit d in number i of length n
(define (narcissistic n)
  ; exponents
  (define expts (for/vector ([i (in-range 10)]) (expt i n)))
  
  ; upper bound on n
  (define bound (expt 10 n))
  
  ; find all numbers
  (sort
   ; min - only add additional digits >= this
   ; digits - digits added so far
   ; sum - the current sum of powers
   (let loop ([min 0] [digits '()] [sum 0])
     (cond 
       ; if we have enough digits, check for a valid solution
       [(= n (length digits))
        (if (equal? digits (sort (digits-of sum) >))
            (list sum)
            '())]
       
       ; if the sum is too large, bail out
       [(>= sum bound)
        '()]
       
       ; otherwise, recur on all possible digits (>= min)
       [else
        (for*/list ([i (in-range min 10)]
                    [res (in-list (loop i
                                        (cons i digits) 
                                        (+ (vector-ref expts i) sum)))])
          res)]))
   <))
               
; find all narcissistic numbers
(define (all-narcissistic)
  ; collect statistics
  (define total-time 0)
  (define total-count 0)
  
  ; try all of them, upper bound = 60 because n*9^n < 10^(n-1) for n <= 60
  (for ([n (in-range 1 61)])
    ; run this example
    (define-values (ans cpu-time real-time gc-time) 
      (time-apply narcissistic (list n)))
    
    ; add the time (if we got answers or not) and the count
    (set! total-time (+ total-time cpu-time))
    (set! total-count (+ total-count (length (car ans))))
    
    ; print results if we had any
    (when (not (null? (car ans)))
      (printf "~a: ~a ms (~a ms total), ~a value(s)\n~a\n\n" 
              n 
              cpu-time
              total-time
              (length (car ans))
              (car ans))))
  
  ; print out the final results
  (printf " total time: ~a ms\n" total-time)
  (printf "total count: ~a\n" total-count))