#lang racket

(provide n-gram-frequency)

; Calculate n gram frequencies
(define (n-gram-frequency [in (current-input-port)] #:n [n 4] #:limit [limit 100])
  
  ; Store counts and total to do frequency later
  (define counts (make-hash))
  
  ; Keep a circular buffer of text, read char by char
  (define n-gram (make-string 4 #\nul))
  (for ([c (in-port read-char in)])
    (set! n-gram (substring (string-append n-gram (string c)) 1))
    (hash-set! counts n-gram (add1 (hash-ref counts n-gram 0))))

  ; Find the top limit many values
  (define top-n 
    (take 
     (sort
      (for/list ([(key val) (in-hash counts)])
        (list val key))
      (lambda (a b) (> (car a) (car b))))
     limit))
  
  ; Cacluate the frequency of just those
  (define total (* 1.0 (for/sum ([vk (in-list top-n)]) (car vk))))
  (for/hash ([vk (in-list top-n)])
    (values (cadr vk) (/ (car vk) total))))