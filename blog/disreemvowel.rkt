#lang racket

; For this case, y/w are not vowels. Also, latin characters only :)
(define vowels '(#\a #\e #\i #\o #\u))
(define (char-vowel? c) (member c vowels))

; Load a dictionary as a trie, cache the result
(define dictionary
  (let ([rhash #f])
    (thunk
      (when (not rhash)
        (set! rhash (make-hash))
        (with-input-from-file "enable1.txt"
          (thunk
            (for ([word (in-lines)])
              (hash-ref!
               (for/fold ([rhash rhash]) ([c (in-string word)])
                 (hash-ref! rhash c (make-hash)))
               'end
               #t)))))
      rhash)))

; Remove all vowels and non-alphabetic characters
(define (disemvowel str)
  (list->string
   (for/list ([c (in-string str)] 
              #:when (and (char-alphabetic? c)
                          (not (char-vowel? c))))
     c)))

; Take a string of characters sans vowels and figure out all possible phrases that it could have been
(define (reemvowel str)
  (let loop ([chars (string->list str)]
             [current-word '()]
             [current-phrase '()]
             [current-dict (dictionary)])
    (apply 
     append
     ; Base case, used the entire input, no pending words
     (if (and (null? chars)
              (null? current-word))
         (list (string-join (reverse current-phrase) " "))
         '())
     ; If we're at end of a word, add it and onwards
     (if (and (hash-ref current-dict 'end #f)
              (not (andmap char-vowel? current-word)))
         (loop chars 
               '() 
               (cons (list->string (reverse current-word)) current-phrase)
               (dictionary))
         '())
     ; Recur if we have a next non-vowel
     (if (and (not (null? chars))
              (hash-ref current-dict (car chars) #f))
         (loop (cdr chars) 
               (cons (car chars) current-word) 
               current-phrase
               (hash-ref current-dict (car chars)))
         '())
     ; Check any vowel-only conditions by recurring
     (for/list ([c (in-list vowels)]
                #:when (hash-has-key? current-dict c))
       (loop chars 
             (cons c current-word)
             current-phrase
             (hash-ref current-dict c))))))

; Take a string of characters sans vowels and figure out a possible phrase
(define (reemvowel/first str)
  (let/ec return
    (let loop ([chars (string->list str)]
               [current-word '()]
               [current-phrase '()]
               [current-dict (dictionary)])
      ; Out of input and no current word? Found a phrase!
      (when (and (null? chars)
                 (null? current-word))
        (return (string-join (reverse current-phrase) " ")))
      ; If we're at end of a word, add it and onwards
      (when (and (hash-ref current-dict 'end #f)
                 (not (andmap char-vowel? current-word)))
        (loop chars 
              '() 
              (cons (list->string (reverse current-word)) current-phrase)
              (dictionary)))
      ; Recur if we have a next non-vowel
      (when (and (not (null? chars))
                 (hash-ref current-dict (car chars) #f))
        (loop (cdr chars) 
              (cons (car chars) current-word)
              current-phrase
              (hash-ref current-dict (car chars))))
      ; Check any vowel-only conditions by recurring
      (for ([c (in-list (shuffle vowels))]
            #:when (hash-has-key? current-dict c))
        (loop chars 
              (cons c current-word)
              current-phrase 
              (hash-ref current-dict c))))))
