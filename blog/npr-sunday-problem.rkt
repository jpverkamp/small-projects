#lang racket

; load word list from this file
(define DICTIONARY "wordsEn.txt")

; test if _str_ contains the character _char_
(define (string-contains? str char)
  (call/cc 
   (lambda (return)
     (for ([c (in-string str)])
       (when (eq? c char)
         (return #t)))
     #f)))

; find all words containing _chars_ with _length_ characters
(define (words-containing chars [length (length chars)])
  (with-input-from-file DICTIONARY
    (lambda ()
      (for/list ([word (in-lines)]
                 #:when (and (= length (string-length word))
                             (andmap (lambda (c) (string-contains? word c)) chars)))
        word))))