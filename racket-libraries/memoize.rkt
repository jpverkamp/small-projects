#lang racket

(provide define-memoized)

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

; example, fibonacci without memoization
(define (fib n)
  (cond
    [(< n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

; example, fibonacci with memoization
(define-memoized (mfib n)
  (cond
    [(< n 1) 1]
    [else (+ (mfib (- n 1)) (mfib (- n 2)))]))