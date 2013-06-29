#lang racket

; Rotate in the complex numbers
(define (f1 n)
  (* n 0+1i))

; Only negate odd numbers, offset by +- 1
(define (f2 n)
  (define e? (even? n))
  (define p? (positive? n))
  (define n? (negative? n))
  (cond
    [(and e? n?) (+ n 1)]
    [(and e? p?) (- n 1)]
    [n?          (+ (- n) 1)]
    [p?          (- (- n) 1)]
    [else        0]))

; Wrap as a string, negate on conversion back
(define (f3 n)
  (cond
    [(string? n) (- (string->number n))]
    [else        (number->string n)]))

; Use a state variable
(define f4
  (let ([toggle (make-parameter #t)])
    (lambda (n)
      (toggle (not (toggle)))
      (if (toggle) n (- n)))))

(module+ test
  (require rackunit)
  (for ([f (in-list (list f1 f2 f3 f4))])
    ; Positive integers
    (for ([i (in-range 10)])
      (define n (random 100))
      (check-equal? (f (f n)) (- n) (format "~s(~s(~s))" f f n)))
    
    ; Negative integers
    (for ([i (in-range 10)])
      (define n (- (random 100)))
      (check-equal? (f (f n)) (- n) (format "~s(~s(~s))" f f n)))
    
    ; Zero and +- 1
    (for ([n (in-list '(-1 0 1))])
      (check-equal? (f (f n)) (- n) (format "~s(~s(0))" f f)))))