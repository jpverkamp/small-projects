#lang racket

(provide
 word-squares
 print-word-square)

(require "dictionary.rkt")

; cons to the end, this is probably not something we want
; just keep it small
(define (snoc x ls)
  (reverse (cons x (reverse ls))))

; given a dictionary and a word, create a word square
; it should be len(word) words, each starting with a letter in word
; example: 
;   AURORA
;   UNEVEN
;   REFUND
;   OVULAR
;   RENAME
;   ANDREW
(define (word-squares dict word)
  ; start with just the one word
  (let loop ([words (list (string-upcase word))])
    (if (= (length words) (string-length word))
        (list words)
        ; get a prefix for the new row
        (let ([prefix (list->string 
                       (for/list ([i (length words)])
                         (string-ref (list-ref words i) 
                                     (length words))))])
          ; branch into each of those cases
          ; filter the words based on length
          (apply
           append
           (for/list ([next (filter 
                             (? (each) (= (string-length each) 
                                          (string-length word)))
                             (words-by-prefix dict prefix))]
                      #:when (not (null? next)))
             (loop (snoc next words))))))))

; print out a word square
(define (print-word-square square)
  (for-each
   (? (word) (printf "~a\n" word))
   square)
  (newline))