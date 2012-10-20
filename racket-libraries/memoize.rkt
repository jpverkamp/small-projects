#lang racket

(provide define-memoized)

; replace define with a memoized version
(define-syntax define-memoized-old
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

; replace define with a memoized version
(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       ; store the cache as a hash of args => result       
       (let ([cache (make-hash)])
         (lambda (args ...)
           ; if we haven't calculated it before, do so now
           (when (not (hash-has-key? cache `(,args ...)))
             (hash-set! cache `(,args ...) (begin bodies ...)))
           ; return the cached result
           (hash-ref cache `(,args ...)))))]))
       
; example, fibonacci without memoization
(define (fib n)
  (cond
    [(< n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

; example, fibonacci with memoization
(define-memoized-old (mfib n)
  (cond
    [(< n 1) 1]
    [else (+ (mfib (- n 1)) (mfib (- n 2)))]))

; example, fibonacci with memoization using the second macro
(define-memoized (mfib2 n)
  (cond
    [(< n 1) 1]
    [else (+ (mfib2 (- n 1)) (mfib2 (- n 2)))]))

(define n 100000)
(collect-garbage)
(time (let ([result (mfib n)]) n))
(collect-garbage)
(time (let ([result (mfib2 n)]) n))