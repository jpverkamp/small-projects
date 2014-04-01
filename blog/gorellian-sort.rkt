#lang racket

; Sort characters by a non-standard alphabet
(define (make-gorellian-char<? alphabet)
  (λ (c1 c2)
    ; If we match either, we have an answer
    ; If we never match, they're = (and thus not <)
    (for/first ([c (in-string alphabet)]
                #:when (or (eq? c c1) (eq? c c2)))
      ; If we matched c1 first, the c1 is smaller
      (eq? c c1))))

; Sort strings by a non-standard alphabet
(define (make-gorellian-string<? alphabet)
  (define char<? (make-gorellian-char<? alphabet))
  (λ (str1 str2)
    (for/first ([c1 (in-string str1)]
                [c2 (in-string str2)]
                #:unless (eq? c1 c2))
      (char<? c1 c2))))

(module+ test
  (require rackunit)
  
  ; Run test cases in the format specified here:
  ; http://www.reddit.com/r/dailyprogrammer/comments/20sjif/4192014_challenge_154_intermediate_gorellian/
  (define (test [in (current-input-port)])
    (define sample-count (read in))
    (define alphabet (symbol->string (read in)))
    (define string<? (make-gorellian-string<? alphabet))
    (define words
      (for/list ([i (in-range sample-count)])
        (symbol->string (read in))))
    (sort words string<?))
  
  (check-equal? 
   (call-with-input-string 
    "
8 UVWXYZNOPQRSTHIJKLMABCDEFG
ANTLER
ANY
COW
HILL
HOW
HOWEVER
WHATEVER
ZONE
"
   test)
   '("WHATEVER" "ZONE" "HOW" "HOWEVER" "HILL" "ANY" "ANTLER" "COW"))
  
  (check-equal?
   (call-with-input-string 
    "
5 zZyYxXwWvVuUtTsSrRqQpPoOnNmMlLkKjJiIhHgGfFeEdDcCbBaA
go
aLL
ACM
teamS
Go
"
    test)
   '("teamS" "go" "Go" "aLL" "ACM")))
