#lang racket

(provide gregorian->mayan
         mayan->gregorian)

; date structures
(define-struct gregorian (year month day) #:transparent)
(define-struct mayan (baktun katun tun uinal kin) #:transparent)

; test divisibility
(define (divisible? m n)
  (zero? (remainder m n)))

; test if a year is a leap year
(define (leap-year? year)
  (or (and (divisible? year 4)
           (not (divisible? year 100)))
      (divisible? year 400)))

; days per month
(define days/month '(31 28 31 30 31 31 30 31 30 31 30 31))

; convert from gregorian to days since 1 jan 1970
(define (gregorian->days date)
  ; a date after 1 jan 1970?
  (define go-> (>= (gregorian-year date) 1970))
  
  ; are we after February?
  (define feb+ (> (gregorian-month date) 2))
  
  ; range for leap years to test
  (define leap-range
    (list
     (if go-> 1970 (+ (gregorian-year date) (if feb+ 0 1)))
     (if go-> (+ (gregorian-year date) (if feb+ 1 0)) 1971)))
  
  (+ ; add year
     (* 365 (- (gregorian-year date) (if go-> 1970 1969)))
     ; add month
     (* (if go-> 1 -1) 
        (apply + ((if go-> take drop) days/month (- (gregorian-month date) 1))))
     ; add day
     (- (gregorian-day date) 1)
     ; deal with leap years
     (for/sum ([year (apply in-range leap-range)])
       (if (leap-year? year) (if go-> 1 -1) 0))))

; convert from days since 1 jan 1970 to gregorian date
(define (days->gregorian days)
  (cond
    ; work forward from 1 jan 1970
    [(> days 0)
     (let loop ([days days] [year 1970] [month 1] [day 1])
       ;(printf "~s  ~s-~s-~s\n" days year month day)
       (define d/y (if (leap-year? year) 366 365))
       (define d/m (if (and (leap-year? year) (= month 2))
                       29
                       (list-ref days/month (- month 1))))
       (cond
         [(>= days d/y)
          (loop (- days d/y) (+ year 1) month day)]
         [(>= days d/m)
          (loop (- days d/m) year (+ month 1) day)]
         [else
          (make-gregorian year month (+ day days))]))]
    ; work backwards from 1 jan 1970
    [(< days 0)
     (let loop ([days (- (abs days) 1)] [year 1969] [month 12] [day 31])
       (define d/y (if (leap-year? year) 366 365))
       (define d/m (if (and (leap-year? year) (= month 2))
                       29
                       (list-ref days/month (- month 1))))
       (cond
         [(>= days d/y)
          (loop (- days d/y) (- year 1) month day)]
         [(>= days d/m)
          (loop (- days d/m) year (- month 1) (list-ref days/month (- month 2)))]
         [else
          (make-gregorian year month (- d/m days))]))]
    ; that was easy
    [else
     (make-gregorian 1970 1 1)]))

; convert from mayan to days since 1 jan 1970
(define (mayan->days date)
  (+ -1856305
     (mayan-kin date)
     (* 20 (mayan-uinal date))
     (* 20 18 (mayan-tun date))
     (* 20 18 20 (mayan-katun date))
     (* 20 18 20 20 (mayan-baktun date))))

; convert from days since 1 jan 1970 to a mayan date
(define (days->mayan days)
  (define-values (baktun baktun-days) (quotient/remainder (+ days 1856305) (* 20 18 20 20)))
  (define-values (katun katun-days) (quotient/remainder baktun-days (* 20 18 20)))
  (define-values (tun tun-days) (quotient/remainder katun-days (* 20 18)))
  (define-values (uinal kin) (quotient/remainder tun-days 20))
  (make-mayan baktun katun tun uinal kin))

; convert from gregorian to mayan
(define (gregorian->mayan date)
  (days->mayan (gregorian->days date)))

; convert from mayan to gregorian 
(define (mayan->gregorian date)
  (days->gregorian (mayan->days date)))

; do some testing
(require rackunit)
(for ([test (in-list '((( 741  6 28)  -448705 ( 9 15 10  0  0))
                       ((1900  1  1)  -25567  (12 14  5  6 18))
                       ((1969  5 17)  -229    (12 17 15 13 16))
                       ((1970  1  1)   0      (12 17 16  7  5))
                       ((1970 10  1)   273    (12 17 17  2 18))
                       ((1987 10  1)   6482   (12 18 14  7  7))
                       ((1989  1 17)   6956   (12 18 15 13  1))
                       ((2012 12 21)   15695  (13  0  0  0  0))
                       ((2013  1 25)   15730  (13  0  0  1 15))))])
  
  (define g (apply make-gregorian (car test)))
  (define d (cadr test))
  (define m (apply make-mayan (caddr test)))
  
  (check-equal? (gregorian->days g) d)
  (check-equal? (days->gregorian d) g)
  (check-equal? (days->gregorian (gregorian->days g)) g)  
  
  (check-equal? (mayan->days m) d)
  (check-equal? (days->mayan d) m)
  (check-equal? (days->mayan (mayan->days m)) m)
  
  (check-equal? (gregorian->mayan g) m)
  (check-equal? (mayan->gregorian m) g))