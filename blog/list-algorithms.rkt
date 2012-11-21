#lang racket

; Calculate the intersection of two lists using nested loops.
(define (intersection-loops ls1 ls2)
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) '()]
      [(member (car ls1) ls2) 
       (cons (car ls1) (loop (cdr ls1)))]
      [else
       (loop (cdr ls1))])))

; Calculate the union of two lists using nested loops.
(define (union-loops ls1 ls2)
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) ls2]
      [(member (car ls1) ls2) 
       (loop (cdr ls1))]
      [else
       (cons (car ls1) (loop (cdr ls1)))])))

; Calculate the difference of two lists using nested loops.
(define (difference-loops ls1 ls2)
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) '()]
      [(member (car ls1) ls2) 
       (loop (cdr ls1))]
      [else
       (cons (car ls1) (loop (cdr ls1)))])))

; Calculate the intersection of two lists by pre-sorting.
(define (intersection-sort ls1 ls2)
  (let loop ([ls1 (sort ls1 <)]
             [ls2 (sort ls2 <)])
    (cond
      [(null? ls1) '()]
      [(null? ls2) '()]
      [(< (car ls1) (car ls2))
       (loop (cdr ls1) ls2)]
      [(> (car ls1) (car ls2))
       (loop ls1 (cdr ls2))]
      [else
       (cons (car ls1) (loop (cdr ls1) (cdr ls2)))])))

; Calculate the union of two lists by pre-sorting.
(define (union-sort ls1 ls2)
  (let loop ([ls1 (sort ls1 <)]
             [ls2 (sort ls2 <)])
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(< (car ls1) (car ls2))
       (cons (car ls1) (loop (cdr ls1) ls2))]
      [(> (car ls1) (car ls2))
       (cons (car ls2) (loop ls1 (cdr ls2)))]
      [else
       (cons (car ls1) (loop (cdr ls1) (cdr ls2)))])))

; Calculate the difference of two lists by pre-sorting.
(define (difference-sort ls1 ls2)
  (let loop ([ls1 (sort ls1 <)]
             [ls2 (sort ls2 <)])
    (cond
      [(null? ls1) '()]
      [(null? ls2) ls1]
      [(< (car ls1) (car ls2))
       (cons (car ls1) (loop (cdr ls1) ls2))]
      [(> (car ls1) (car ls2))
       (loop ls1 (cdr ls2))]
      [else
       (loop (cdr ls1) (cdr ls2))])))

; Calculate the intersection of two lists using a hash.
(define (intersection-hash ls1 ls2)
  (define ls2-hash (for/hash ([e2 (in-list ls2)]) (values e2 #t)))
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) '()]
      [(hash-has-key? ls2-hash (car ls1)) 
       (cons (car ls1) (loop (cdr ls1)))]
      [else
       (loop (cdr ls1))])))

; Calculate the union of two lists using a hash.
(define (union-hash ls1 ls2)
  (define ls2-hash (for/hash ([e2 (in-list ls2)]) (values e2 #t)))
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) ls2]
      [(hash-has-key? ls2-hash (car ls1)) 
       (loop (cdr ls1))]
      [else
       (cons (car ls1) (loop (cdr ls1)))])))

; Calculate the difference of two lists using a hash.
(define (difference-hash ls1 ls2)
  (define ls2-hash (for/hash ([e2 (in-list ls2)]) (values e2 #t)))
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) '()]
      [(hash-has-key? ls2-hash (car ls1)) 
       (loop (cdr ls1))]
      [else
       (cons (car ls1) (loop (cdr ls1)))])))


(define (test)
  (for ([size (in-list (map (lambda (n) (* (+ n 1) 10000)) (range 20)))])
    ; generate the lists 
    (define ls1 (take (shuffle (range (* 2 size))) size))
    (define ls2 (take (shuffle (range (* 2 size))) size))
      
    ; test the algorithms
    (for* ([type (in-list '(intersection union difference))]
           [algo (in-list '(loops sort hash))])
      
      
      (define name (string->symbol
                    (string-append (symbol->string type)
                                   "-"
                                   (symbol->string algo))))
      (define f (eval name))
      
      (collect-garbage)
      (define-values (times cpu real gc) (time-apply f (list ls1 ls2)))
      (printf "~s,~s,~s,~s,~s,~s\n" size type algo cpu gc (- cpu gc)))))
