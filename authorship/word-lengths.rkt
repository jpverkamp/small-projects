#lang racket

(provide word-lengths)

; Calculate the relative frequencies of stop words in a text
(define (word-lengths [in (current-input-port)])
  ; Store the length counts
  (define counts (make-hash))
  
  ; Count all of the base words in the text
  (for* ([line (in-lines in)]
         [word (in-list (string-split line))])
    (define len (string-length word))
    (hash-set! counts len (add1 (hash-ref counts len 0))))
  
  ; Normalize
  (define total (* 1.0 (for/sum ([(k v) (in-hash counts)]) v)))
  (for/hash ([(k v) (in-hash counts)])
    (values k (/ v total))))