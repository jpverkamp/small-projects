#lang racket

; given an ordered list of numbers, a list of ops on
; them, and a target return all interspaced lists that
; evaluate to the given target
(define (solve nums ops target)
  (for ([expr (in-list (generate nums (map car (apply append ops))))]
        #:when (= (evaluate expr ops) target))
    (printf "~s\n" expr)))

; generate a list of expressions given an ordered list
; of values and a list of possible operations between
; them
(define (generate nums ops)
  (cond
    [(null? (cdr nums))
     (list nums)]
    [else
     (for*/list ([op (in-list ops)]
                 [res (in-list (generate (cdr nums) ops))])
       (list* (car nums) op res))]))

; evaulate an infix expression given a list of ops
; ops is a list of pairs of symbol -> binary function
; example: (((* *) (/ div))
;           ((+ +) (- -)))
(define (evaluate expr ops)
  ; step through each level of ops
  (define (step expr ops)
    (cond
      [(null? ops)
       expr]
      [else
       (step (reduce expr (car ops)) (cdr ops))]))
  
  ; reduce one level 
  (define (reduce expr ops)
    (cond
      [(or (null? expr)
           (null? (cdr expr))
           (null? (cddr expr)))
       expr]
      [(assoc (cadr expr) ops)
       => (lambda (pair)
            (reduce (cons ((cadr pair) (car expr) (caddr expr)) (cdddr expr)) ops))]
      [else
       (cons (car expr) (reduce (cdr expr) ops))]))
  
  ; start out the main loop
  (car (step expr ops)))

; solve this year's problem
(define (solve-2013-countdown)
  (solve '(10 9 8 7 6 5 4 3 2 1)
         `(((~ ,(lambda (x y) (string->number (string-append (number->string x) (number->string y))))))
           ((* ,*) (/ ,/))
           ((+ ,+) (- ,-)))
         2013))
