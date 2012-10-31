#lang racket

(provide make-multivector
         multivector-ref
         multivector-set!)

(define-struct multivector (size per-chunk data)
  #:mutable
  #:transparent
  #:constructor-name make-multivector-struct)

(define (make-multivector size [chunks 1] [default #f])
  (define per-chunk (inexact->exact (ceiling (/ size chunks))))
  (make-multivector-struct
   size per-chunk
   (for/vector ([i (in-range chunks)])
     (if (= i (- chunks 1))
         (make-vector (- size (* (- chunks 1) per-chunk)) default)
         (make-vector per-chunk default)))))


(define (multivector-ref mv i)
  (vector-ref (vector-ref (multivector-data mv)
                          (quotient i (multivector-per-chunk mv)))
              (remainder i (multivector-per-chunk mv))))

(define (multivector-set! mv i v)
  (vector-set! (vector-ref (multivector-data mv)
                           (quotient i (multivector-per-chunk mv)))
               (remainder i (multivector-per-chunk mv))
               v))

(define (multivector->vector mv)
  (for/vector ([i (in-range (multivector-size mv))])
    (multivector-ref mv i)))