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
     (thread
      (λ ()
        (gzip-through-ports (current-input-port) pipe-to #f (current-seconds))
        (close-output-port pipe-to)))
     (parameterize ([current-input-port pipe-from])
       (thunk)))
   (λ ()
     (unless (port-closed? pipe-to) (close-output-port pipe-to))
     (unless (port-closed? pipe-from) (close-input-port pipe-from)))))

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
