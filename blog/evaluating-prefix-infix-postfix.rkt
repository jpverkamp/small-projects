#lang racket

(provide prefix-calc infix-calc postfix-calc)

; evaluate a prefix expression from a list
(define (prefix-calc main-expr)  
  ; return both car and cdr of a list
  (define (car+cdr ls)
    (values (car ls) (cdr ls)))
  
  ; take a single step down an expression
  (define (step expr)
    (cond
      [(number? (car expr))
       (values (car expr) (cdr expr))]
      [else
       (let*-values ([(op rest) (car+cdr expr)]
                     [(lv rest) (step rest)]
                     [(rv rest) (step rest)])
         (values ((eval op) lv rv) rest))]))
 
  ; main evaluation
  (let-values ([(val rest) (step main-expr)])
    (if (null? rest)
        val
        (error 'prefix-calc 
          (format "malformed expression ~s: ~s left over" main-expr rest)))))

; evaluate an infix expression from a list
(define (infix-calc expr)
  ; take one simplification step
  (define (step ls which)
    (cond
      [(number? ls) ls]
      [(or (null? ls) (null? (cdr ls)) (null? (cddr ls))) ls]
      [(member (cadr ls) which)
       (cons ((eval (cadr ls)) (car ls) (caddr ls)) (cdddr ls))]
      [else
       (cons (car ls) (step (cdr ls) which))]))
  
  ; repeatedly take steps until it's reduced
  (define (step-all ls)
    (let ([step1 (step ls '(* /))])
      (if (equal? ls step1)
          (let ([step2 (step step1 '(+ -))])
            (if (equal? step1 step2)
                (car step2)
                (step-all step2)))
          (step-all step1))))
  
  ; run
  (step-all expr))

; evaluate a postfix expression from a list
(define (postfix-calc expr)
  ; take a step, updating the stack
  (define (step expr stack)
    (cond
      [(null? expr) (car stack)]
      [(number? (car expr))
       (step (cdr expr)
             (cons (car expr) stack))]
      [else
       (step (cdr expr)
             (cons ((eval (car expr))
                    (cadr stack)
                    (car stack))
                   (cddr stack)))]))
  
  ; run
  (step expr '()))