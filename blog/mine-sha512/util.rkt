#lang racket

(provide (all-defined-out))

; Generate a random alphanumeric string
(define random-string
  (let ([alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890"])
    (λ (length)
      (list->string 
       (for/list ([i (in-range length)])
         (string-ref alphabet (random (string-length alphabet))))))))

; Print out current timing information
(define (print-timing id hashes seconds best-string best-hash)
  (define (shorten n)
    (define prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y"))
    (let loop ([n n] [prefixes prefixes])
      (cond
        [(or (null? (cdr prefixes))
             (< n 1000))
         (format "~a ~a" (/ (truncate (* 10.0 n)) 10.0) (car prefixes))]
        [else
         (loop (/ n 1000) (cdr prefixes))])))
  
  (printf "~a: ~a -> ~a... (~ah @ ~ah/s)\n"
          id
          best-string
          (substring best-hash 0 16)
          (shorten hashes)
          (shorten (/ hashes seconds)))
  (flush-output))