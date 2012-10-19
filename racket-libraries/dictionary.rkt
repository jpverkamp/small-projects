#lang racket

(provide
 make-dictionary
 load-dictionary
 add-word!
 contains?
 words-by-prefix
 lookup)

; make the structure opaque in case I try to print them
(define-struct :dictionary: (value))

; make a dictionary
(define (make-dictionary)
  (make-:dictionary: (make-hasheq)))

; add a word to a dictionary (internally a trie)
; keys are either characters => nested trie or 'word => #t/#f
(define (add-word! dict word)
  (let loop ([cs (string->list (string-upcase word))]
             [node (:dictionary:-value dict)])
    (cond
      [(null? cs)
       (hash-set! node 'word #t)]
      [else
       (when (not (hash-ref node (car cs) #f))
         (hash-set! node (car cs) (make-hasheq)))
       (loop (cdr cs) (hash-ref node (car cs)))])))
       
; helper to do a partial lookup in a dictionary
; returns a node in the trie if given a valid prefix/word
; returns #f otherwise
(define (get-node dict word)
  (let loop ([cs (string->list (string-upcase word))]
             [node (:dictionary:-value dict)])
    (or (and (null? cs)
             node)
        (and (not (null? cs))
             node
             (loop (cdr cs) 
                   (hash-ref node (car cs) #f))))))

; check if a word is in the dictionary
(define (contains? dict word)
  (let ([node (get-node dict word)])
    (and node (hash-ref node 'word #f))))
  
; load a dictionary from a file
; one word per line, case doesn't matter
; ignores any words that have non-alphabetic characters
(define (load-dictionary filename)
  (let ([dict (make-dictionary)])
    (with-input-from-file filename
      (? ()
        (let loop ([line (read-line)])
          (unless (eof-object? line)
            (let ([word (string-trim line)])
              (add-word! dict word))
            (loop (read-line))))))
    dict))

; given a dictionary (or it's node (or #f)) and a letter
; return the nested value or #f it there isn't one
(define (lookup dict letter)
  (let ([node (if (:dictionary:? dict) (:dictionary:-value dict) dict)])
    (and node
         (hash-ref node letter #f))))

; get a list of all words in the dictionary with a given prefix
(define (words-by-prefix dict prefix)
  (let ([prefix (string-upcase prefix)])
    ; once we have the suffixes, add our prefix
    (map 
     (? (ls) (string-append prefix (list->string ls)))
     ; loop from the node to all leaves
     (let loop ([node (get-node dict prefix)])
       (if node
           (let ([r (apply 
                     append
                     ; add the current letter to each recursions
                     (for/list ([(k v) node] 
                                #:when (and (not (eq? k 'word)) v))
                       (map (? (ls) (cons k ls))
                            (loop v))))])
             ; include the prefix if it's a word
             (if (hash-ref node 'word #f)
                 (cons '() r)
                 r))
           '())))))
