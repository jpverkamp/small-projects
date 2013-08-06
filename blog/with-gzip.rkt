#lang racket

(require file/gzip 
         file/gunzip)

(provide/contract 
 (with-gzip
  (-> (-> any) #:buffer-size [or/c false? exact-positive-integer?] any)))

(define (with-gzip thunk #:buffer-size [buffer-size #f])
  (define-values (pipe-from pipe-to) (make-pipe buffer-size))
  (with-handlers ([exn:fail?
                   (λ (err)
                     (close-output-port pipe-to)
                     (close-input-port pipe-from)
                     (error 'with-gzip (exn-message err)))])
    (gzip-through-ports (current-input-port) pipe-to #f (current-seconds))
    (close-output-port pipe-to)
    (parameterize ([current-input-port pipe-from])
      (thunk))
    (close-input-port pipe-from)))

(provide/contract 
 (with-gunzip 
  (-> (-> any) #:buffer-size [or/c false? exact-positive-integer?] any)))
                  
(define (with-gunzip thunk #:buffer-size [buffer-size #f])
  (define-values (pipe-from pipe-to) (make-pipe buffer-size))
  (with-handlers ([exn:fail?
                   (λ (err)
                     (close-output-port pipe-to)
                     (close-input-port pipe-from)
                     (error 'with-gunzip (exn-message err)))])
    (gunzip-through-ports (current-input-port) pipe-to)
    (close-output-port pipe-to)
    (parameterize ([current-input-port pipe-from])
      (thunk))
    (close-input-port pipe-from)))