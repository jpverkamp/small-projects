#lang racket

(require file/gzip 
         file/gunzip)

(provide/contract 
 (with-gzip
   (->* ((-> any))
        (#:buffer-size (or/c false? exact-positive-integer?))
        any)))

(define (with-gzip thunk #:buffer-size [buffer-size #f])
  (define-values (pipe-from pipe-to) (make-pipe buffer-size))
  (dynamic-wind
   void
   (λ ()
     (define t (thread (λ () (gzip-through-ports pipe-from (current-output-port) #f (current-seconds)))))
     (parameterize ([current-output-port pipe-to])
       (thunk))
     (close-output-port pipe-to)
     (thread-wait t))
   (λ ()
     (unless (port-closed? pipe-to) (close-output-port pipe-to))
     (unless (port-closed? pipe-from) (close-input-port pipe-from))))
  (void))

(provide/contract 
 (with-gunzip 
   (->* ((-> any))
        (#:buffer-size (or/c false? exact-positive-integer?))
        any)))
                  
(define (with-gunzip thunk #:buffer-size [buffer-size #f])
  (define-values (pipe-from pipe-to) (make-pipe buffer-size))
  (dynamic-wind
   void
   (λ ()
     (thread 
      (λ ()
        (gunzip-through-ports (current-input-port) pipe-to)
        (close-output-port pipe-to)))
     (parameterize ([current-input-port pipe-from])
       (thunk)))
   (λ ()
     (unless (port-closed? pipe-to) (close-output-port pipe-to))
     (unless (port-closed? pipe-from) (close-input-port pipe-from)))))
