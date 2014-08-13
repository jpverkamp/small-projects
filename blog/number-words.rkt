#lang racket

; Given 1-26 mapping to A-Z, determine all possible words represented by a number
; Correctly resolve ambiguities where 1234 -> 1 2 3 4 = ABCD / 1 23 4 -> AWD / 12 3 4 -> LCD
(define (number->words str)
  ; Convert a number 1-26 to a letter A-Z
  (define (n->char n) (integer->char (+ 64 (string->number n))))
  
  ; Make an optional parser 
  ; If the regex matches, add it to each possible next parse
  ; If it does not, return an empty list (to be appendable)
  (define (make-parser re)
    (λ (str)
      (match str
        [(regexp re (list _ n rest))
         (map (curry ~a (n->char n)) (number->words rest))]
        [any 
         '()])))
  
  ; Create parsers for valid 1 digit and 2 digit letter numbers
  (define parse-1 (make-parser #px"([1-9])(.*)"))
  (define parse-2 (make-parser #px"(1[0-9]|2[0-6])(.*)"))
  
  ; Base case, so we can stop eventually
  (if (equal? str "")
      '("")
      (append (parse-1 str) (parse-2 str))))

; Convert words back to numbers
(define (words->number str)
  (define (char->n c) (number->string (- (char->integer c) 64)))
  (apply ~a (for/list ([c (in-string str)]) (char->n c))))
