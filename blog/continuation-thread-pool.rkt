#lang racket

(provide make-empty-thread-pool
         current-thread-pool 
         get-switch-count
         yield
         threads)

; Store the current state of a thread
(define-struct thread-state (continuation) #:mutable)

; Store a collection of threads in a pool
(define-struct thread-pool (all current switch-count) #:mutable)

; Create a new, empty thread pool
(define (make-empty-thread-pool)
  (make-thread-pool '() '() 0))

; Allow for multiple thread pools
(define current-thread-pool 
  (make-parameter (make-empty-thread-pool)))

; Get the number of context switches out of the current thread pool
(define (get-switch-count [pool (current-thread-pool)])
  (thread-pool-switch-count pool))

; Yield control of the current thread to the next thread in the current pool
(define (yield)
  (call-with-current-continuation
   (lambda (k)
     ; Unwrap the current pool
     (define pool (current-thread-pool))
     
     ; Store the current thread state
     (define thread (car (thread-pool-current pool)))
     (set-thread-state-continuation! thread k)
     
     ; Advance to the next thread
     (set-thread-pool-current! pool (cdr (thread-pool-current pool)))
     (when (null? (thread-pool-current pool))
       (set-thread-pool-current! pool (thread-pool-all pool)))
     (set-thread-pool-switch-count! pool (+ 1 (thread-pool-switch-count pool)))
     
     ; Run that thread
     (define next-k (thread-state-continuation (car (thread-pool-current pool))))
     (next-k (void)))))

; Run a given list of thunks in parallel
(define (threads . thunks)
  (call-with-current-continuation
   (lambda (k)
     (when (null? thunks) (error 'threads "must specify at least one thread"))
     
     ; Unwrap the current pool
     (define pool (current-thread-pool))
     
     ; Create the initial thread states
     (define threads
       (map 
        (lambda (thunk) (thread-state (lambda (_) (k (thunk)))))
        thunks))
     
     ; Store them in the pool
     (set-thread-pool-current! pool (append (thread-pool-current pool) threads))
     (set-thread-pool-all! pool (append (thread-pool-all pool) threads))
     
     ; Start the first thread
     (define first-k (thread-state-continuation (car (thread-pool-current pool))))
     (first-k (void)))))

; Test with fibonacci numbers
(module+ test
  (printf "--- fibonacci test ---\n")
  
  (define (fib n)
    (yield)
    (if (<= n 1)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))
  
  (parameterize ([current-thread-pool (make-empty-thread-pool)])
    (printf "fib test returned: ~a\n" 
            (time
             (threads
              (lambda () (list 10 (fib 10)))
              (lambda () (list 20 (fib 20))))))
    (printf "~a context switches\n" (get-switch-count))))

; Test to make sure creating new thread pools works
(module+ test
  (printf "--- parameterizable test ---\n")
  
  (parameterize ([current-thread-pool (make-empty-thread-pool)])
    (threads
     (lambda () (printf "it worked!\n")))))

; Test with tick/tock
(module+ test
  (printf "--- tick tock test ---\n")
  (define run-for 0.5)
  (define delay 0.001)
  
  (parameterize ([current-thread-pool (make-empty-thread-pool)])
    (time 
     (threads 
      ; Stop after 1 second
      (lambda ()
        (let loop ([i 0])
          (sleep delay)
          (when (< (* i delay) run-for)
            (yield)
            (loop (+ i 1)))))
      ; Keep printing tick
      (lambda ()
        (let loop ()
          (printf "tick\n")
          (yield)
          (loop)))
      ; Keep printing tock
      (lambda ()
        (let loop ()
          (printf "tock\n")
          (yield)
          (loop)))))
    (printf "~a context switches\n" (get-switch-count))))

; Test context switches
(module+ test
  (printf "--- context switch test ---\n")
  (define iterations 1000)
  
  (parameterize ([current-thread-pool (make-empty-thread-pool)])
    (time
     (threads
      (lambda () 
        (let loop ([i 0])
          (when (< i iterations)
            (yield)
            (loop (+ i 1)))))))
    (printf "~a context switches (target: ~a)\n" 
            (get-switch-count)
            iterations)))

; Test nested thead pools.
(module+ test
  (printf "--- nested test ---\n")
  
  (parameterize ([current-thread-pool (make-empty-thread-pool)])
    (threads
     (lambda ()
       (parameterize ([current-thread-pool (make-empty-thread-pool)])
         (threads
          (lambda () (let loop () (printf "tick\n") (yield) (loop)))
          (lambda () (let loop () (printf "tock\n") (yield) (loop))))))
     (lambda ()
       (parameterize ([current-thread-pool (make-empty-thread-pool)])
         (threads
          (lambda () (list 10 (fib 10)))
          (lambda () (list 20 (fib 20)))))))))