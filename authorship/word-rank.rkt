#lang racket

(provide word-rank)

; Remove non word characters
(define (fix-word word)
  (list->string 
   (for/list ([c (in-string word)] 
              #:when (char-alphabetic? c)) 
     (char-downcase c))))

; Calculate the relative frequencies of stop words in a text
(define (word-rank [in (current-input-port)] #:limit [limit 100])
  ; Store the word counts
  (define counts (make-hash))
  
  ; Count all of the base words in the text
  (for* ([line (in-lines in)]
         [word (in-list (string-split line))])
    (define fixed (fix-word word))
    (hash-set! counts fixed (add1 (hash-ref counts fixed 0))))
  
  ; Extract the top limit words
  (define top-n
    (map first
         (take
          (sort 
           (for/list ([(word count) (in-hash counts)])
             (list word count))
           (lambda (a b) (> (second a) (second b))))
          limit)))
  
  ; Add an order to each, descending
  ; Return as a hash
  (for/hash ([i (in-range limit 0 -1)]
             [word (in-list top-n)])
    (values word i)))
    