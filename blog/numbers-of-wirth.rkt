#lang racket

#|
Develop a program that generates in ascending order the least 100 numbers of the
set M, where M is defined as follows:

a) The number 1 is in M.

b) If x is in M, then y = 2 * x + 1 and z = 3 * x + 1 are also in M.

c) No other numbers are in M.

-- Systematic Programming: An Introduction, exercise 15.12
|#

; --- ORIGINAL SOLUTION ---

; recursively determine if a number is a wirth number
(define (wirth? n)
  (and (positive? n)
       (integer? n)
       (or (= n 1)
           (wirth? (/ (- n 1) 2))
           (wirth? (/ (- n 1) 3)))))

; list the first n wirth numbers
(define (n-wirth n)
  (let loop ([i 1] [cnt 0])
    (cond
      [(= cnt n) '()]
      [(wirth? i) (cons i (loop (+ i 1) (+ cnt 1)))]
      [else (loop (+ i 1) cnt)])))

; --- WITH MEMOIZATION ---

; replace define with a memoized version
(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       ; store the cache as a hash of args => result
       (let ([results (make-hash)])
         ; need to do this to capture both the names and the values
         (lambda (args ...)
           ((lambda vals
              ; if we haven't calculated it before, do so now
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              ; return the cached result
              (hash-ref results vals))
            args ...))))]))

; recursively determine if a number is a wirth number
(define-memoized (mem-wirth? n)
  (and (positive? n)
       (integer? n)
       (or (= n 1)
           (mem-wirth? (/ (- n 1) 2))
           (mem-wirth? (/ (- n 1) 3)))))

; list the first n wirth numbers
(define (mem-n-wirth n)
  (let loop ([i 1] [cnt 0])
    (cond
      [(= cnt n) '()]
      [(mem-wirth? i) (cons i (loop (+ i 1) (+ cnt 1)))]
      [else (loop (+ i 1) cnt)])))

; --- with a sieve ---

; list the first n wirth numbers with a sieve
(define (sieve-n-wirth n sieve-size)
  (define sieve (make-vector sieve-size #f))
  (vector-set! sieve 1 #t)
  (take
   (for/list ([i (in-range sieve-size)]
              #:when (vector-ref sieve i))
     (when (< (+ (* 2 i) 1) sieve-size)
       (vector-set! sieve (+ (* 2 i) 1) #t))
     (when (< (+ (* 3 i) 1) sieve-size)
       (vector-set! sieve (+ (* 3 i) 1) #t))
     i)
   n))

; --- with a heap

(require data/heap)

; list the first n wirth numbers with a heap
(define (heap-n-wirth n)
  (define heap (make-heap <))
  (heap-add! heap 1)
  (let loop ([n n])
    (cond
      [(zero? n) '()]
      [else
       (define i (heap-min heap))
       (heap-add! heap (+ (* 2 i) 1))
       (heap-add! heap (+ (* 3 i) 1))
       (let loop () ; remove duplicates
         (when (= i (heap-min heap))
           (heap-remove-min! heap)
           (loop)))
       (cons i (loop (- n 1)))])))
        