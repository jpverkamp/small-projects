#lang racket

(provide make-bitvector 
         bitvector-ref
         bitvector-set!
         bitvector-toggle!
         bitvector->string)

; create a bitvector
(define (make-bitvector size [default #f])
  (make-bytes (ceiling (/ size 8)) (if default #xff #x00)))

; reference a value from a bitvector
(define (bitvector-ref bv i)
  (not (= 0 (bitwise-and 
             (bytes-ref bv (quotient i 8))
             (arithmetic-shift 1 (remainder i 8))))))

; set a value in a bit vector
(define (bitvector-set! bv i v)
  (bytes-set!
   bv
   (quotient i 8)
   (cond
     [v
      ; set the value by or'ing with 1
      (bitwise-ior (bytes-ref bv (quotient i 8))
                   (arithmetic-shift 1 (remainder i 8)))]
     [else
      ; unset the value by anding with all 1s but a 0 at the interesting point
      (bitwise-and (bytes-ref bv (quotient i 8))
                   (bitwise-xor #xFF
                                (arithmetic-shift 1 (remainder i 8))))])))

; shortcut to toggle a bit in a bitvector
(define (bitvector-toggle! bv i)
  (bitvector-set! bv i (not (bitvector-ref bv i))))

; used to print out a bitvector
; the size is necessary to avoid printing the extra bits
(define (bitvector->string bv size)
  (list->string
   (for/list ([i (in-range size)])
     (if (bitvector-ref bv i) #\1 #\0))))
