#lang rackjure

(require (only-in rnrs/base-6 mod))

(define (caesar str n)
  (define A (char->integer #\A))
  (define a (char->integer #\a))
  (list->string
   (for/list ([c (in-string str)])
     (cond
       [(char<=? #\A c #\Z)
        (~> c char->integer (- A) (+ n) (mod 26) (+ A) integer->char)]
       [(char<=? #\a c #\z)
        (~> c char->integer (- a) (+ n) (mod 26) (+ a) integer->char)]
       [else
        c]))))
