#lang racket

(provide stop-word-frequency)

; Remove non word characters
(define (fix-word word)
  (list->string 
   (for/list ([c (in-string word)] 
              #:when (char-alphabetic? c)) 
     (char-downcase c))))

; Load the stop words
(define stop-words
  (with-input-from-file "stop-words.txt"
    (lambda ()
      (for/set ([line (in-lines)]) (fix-word line)))))

; Calculate the relative frequencies of stop words in a text
(define (stop-word-frequency [in (current-input-port)])
  ; Store the frequency and word count
  (define counts (make-hash))
  (define total (make-parameter 0.0))
  
  ; Loop across the input text
  (for* ([line (in-lines in)]
         [word (in-list (string-split line))])
    (define fixed (fix-word word))
    
    (when (set-member? stop-words fixed)
      (total (+ (total) 1))
      (hash-set! counts word (add1 (hash-ref counts word 0)))))
  
  ; Normalize and return frequencies
  ; Use the order in the stop words file
  (for/vector ([word (in-set stop-words)])
    (/ (hash-ref counts word 0)
       (total))))