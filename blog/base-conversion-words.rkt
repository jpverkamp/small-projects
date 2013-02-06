#lang racket

(provide ->all-bases 
         ->all-base-words 
         scan)

; order of characters for the encoding
(define encoding 
  (string-append
   "0123456789"
   "abcdefghijklmnopqrstuvwxyz"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "+/"))

; convert a number into a character
(define n->c 
  (for/hash ([i (in-naturals)]
             [c (in-string encoding)])
    (values i c)))

; convert a character into a number
(define c->n 
  (for/hash ([i (in-naturals)]
             [c (in-string encoding)])
    (values c i)))

; convert a list of digits in decimal form into a string
(define (dlist->string dls)
  (list->string
   (for/list ([n (in-list dls)])
     (hash-ref n->c n))))

; convert a string into a list of digits in decimal form
(define (string->dlist str)
  (for/list ([c (in-string str)])
    (hash-ref c->n c)))

; convert a digit list in base b to number
(define (->decimal dls b)
  (let loop ([dls dls] [n 0])
    (cond
      [(null? dls) n]
      [else
       (loop (cdr dls) (+ (* n b) (car dls)))])))

; convert a decimal number to a digital list in base b
(define (decimal-> n b)
  (let loop ([n n] [dls '()])
    (cond
      [(= n 0) dls]
      [else
       (loop (quotient n b) (cons (remainder n b) dls))])))

; get all basis conversions for a number
(define (->all-bases n)
  (for/list ([b (in-range 2 65)])
    (dlist->string (decimal-> n b))))

; get a list of all bases that are also words
(define (->all-base-words dict n)
  (filter
   (lambda (word) (contains? dict word))
   (->all-bases n)))

; scan for numbers that turn into words
(define (scan dict)
  (for ([i (in-naturals)])
    (for ([b (in-range 2 65)]
          [word (in-list (->all-bases i))])
      (when (contains? dict word)
        (printf "~s,~s,~s\n" i b word)))))

