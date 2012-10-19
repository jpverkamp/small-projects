#lang racket

(provide
 word-cube)

(require "dictionary.rkt")

; get just unique elements
(define (unique l)
  (foldl (? (x l) (if (member x l) l (cons x l))) '() l))

; given a gride of letters, find all words of at least length 4 with the center
; 
; example:
;   NCB
;   CIO
;   UNE 
; I is required
(define (word-cube dict letters)
  ; setup, sort the list descending so we end up in alphabetical order
  ; assume the 'required' letter is at the midpoint
  (let ([letters (sort (string->list (string-upcase letters)) char>?)]
        [required (char-upcase (string-ref 
                                letters 
                                (quotient (string-length letters) 2)))])
    ; return strings
    (map
     list->string
     ; filter out words that are too short or don't contain the middle letter
     (filter 
      (? (word) (and (>= (length word) 4) (member required word)))
      ; recursively build all words using only the letters given
      (let loop ([letters letters]
                 [word '()]
                 [node dict])
        (apply
         append
         ; loop over the letters
         (let ([r (for/list ([l (unique letters)] #:when (lookup node l))
                    (loop (remv l letters) (cons l word) (lookup node l)))])
           ; return this word if it's an endpoint
           (if (lookup node 'word)
               (cons (list (reverse word)) r)
               r))))))))

; load the default dictionary
(define dict (load-dictionary "wordsEn.txt"))