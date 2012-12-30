#lang racket

(require rackunit)

; generate all strings from A of length N
(define (combinations n a)
  (map list->string
       (let loop ([n n])
         (cond
           [(zero? n) '(())]
           [else
            (for*/list ([c (in-string a)]
                        [r (in-list (loop (- n 1)))])
              (cons c r))]))))

(test-equal? 
 "Test combinations"
 (combinations 3 "ab")
 (list "aaa" "aab" "aba" "abb" "baa" "bab" "bba" "bbb"))

; check if a string has a non-empty repeating substring
(define (repeats? s)
  (call/cc 
   (lambda (return)
     (define N (string-length s))
     (for* ([i (in-range 1 N)]
            [j (in-range 1 (+ 1 (min i (- N i))))])
       (when (equal? (substring s (- i j) i)
                     (substring s i (+ i j)))
         (return #t)))
     (return #f))))

(test-true "repeats tests" (repeats? "abcbc"))
(test-true "repeats tests" (repeats? "ababc"))
(test-true "repeats tests" (repeats? "abbca"))
(test-false "repeats tests" (repeats? "abcba"))
(test-true "repeats tests" (repeats? "dabcabcd"))

; generate all strings of length N from alphabet A with no repeating substrings
(define (non-repeating-direct N A)
  (for/list ([s (in-list (combinations N A))]
             #:when (not (repeats? s)))
    s))

(test-equal?
 "non-repeating-directs"
 (non-repeating-direct 5 "abc")
 '("abaca" "abacb" "abcab" "abcac" "abcba"
   "acaba" "acabc" "acbab" "acbac" "acbca"
   "babca" "babcb" "bacab" "bacba" "bacbc"
   "bcaba" "bcabc" "bcacb" "bcbab" "bcbac"
   "cabac" "cabca" "cabcb" "cacba" "cacbc"
   "cbabc" "cbaca" "cbacb" "cbcab" "cbcac"))