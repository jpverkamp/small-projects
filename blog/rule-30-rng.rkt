#lang racket

(provide make-rng)

; convert a decimal number to a binary list
; 10 => '(1 0 1 0)
(define (dec->bin d)
  (reverse 
   (let loop ([d d])
     (if (= d 0)
         '()
         (cons (remainder d 2) (loop (quotient d 2)))))))
 
; convert a binary list to a decimal number
; '(1 0 1 0) => 10
(define (bin->dec b)
  (let loop ([b (reverse b)] [p 1])
    (if (null? b)
        0
        (+ (* (car b) p) (loop (cdr b) (* p 2))))))

; pad a list out to a given length with a given value
(define (pad-to n ls with)
  (append (make-list (- n (length ls)) with) ls))

; wrap the edges of a vector around rather than returning an error
(define (wrapped-vector-ref v i)
  (vector-ref v (remainder (+ i (vector-length v)) (vector-length v))))

; create a new random number generator
; rule is the rule to use to generate the next state
; width is the number of bits in the generator
; seed is the initial value for the generator
(define (make-rng [rule 30] [width 32] [seed #f])
  ; start by converting the initial state into an internal vector
  ; and the rule into a function that will convert states
  (let ([state 
         (list->vector 
          (pad-to width 
                  (if seed
                      (dec->bin seed)
                      (for/list ([i width]) (random 2)))
                  0))]
        [rule 
         (let ([rule (list->vector (reverse (pad-to 8 (dec->bin rule) 0)))])
           (lambda (a b c)
             (vector-ref rule (+ (* 4 a) (* 2 b) c))))])
    ; return a thunk that will generate each number
    ; calculate the next state, store it, and return a decimal version
    (lambda ()
      (define next
        (for/vector ([i width])
          (rule (wrapped-vector-ref state (- i 1))
                (wrapped-vector-ref state i)
                (wrapped-vector-ref state (+ i 1)))))
      (set! state next)
      (bin->dec (vector->list next)))))