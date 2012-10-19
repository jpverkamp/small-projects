#lang racket

; filter a file for all lines that return #t on f?
(define (filter-file f? fin fout)
  (with-output-to-file fout
    (lambda ()
      (with-input-from-file fin
        (lambda ()
          (let loop ([word (read-line)])
            (unless (eof-object? word)
              (let ([word (string-trim word)])
                (when (f? word)
                  (printf "~a\n" word)))
              (loop (read-line)))))))
    #:exists 'truncate))

; test if a list is sorted according to a predicate
(define (sorted? < ls)
  (or (null? ls)
      (null? (cdr ls))
      (and (< (car ls) (cadr ls))
           (sorted? < (cdr ls)))))
 
; return only unique elements in a list
(define (unique ls)
  (foldl (lambda (x l) (if (member x l) l (cons x l))) '() ls))

; test if a letter contains all five vowels in ascending order
(define (ascending-vowels? word)
  (let ([vowels (filter
                 (lambda (c) (member c '(#\a #\e #\i #\o #\u)))
                 (string->list word))])
    (and (= (length vowels) 5)
         (= (length (unique vowels)) 5)
         (sorted? char<? vowels))))

; test if a word is made of ascending letters (and at least 6 long)
(define (ascending-letters? word)
  (let ([letters (string->list word)])
    (and (>= (length letters) 6)
         (sorted? char<? letters))))