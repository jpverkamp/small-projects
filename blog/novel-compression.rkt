#lang racket

; Format:
; \d+        ; n = number of words in the dictionary
; (word){n}  ; dictionary
; ( \d+      ; word from the dictionary
; | \d+\^    ; dictionary word, upcase the first letter
; | \d+!     ; dictionary word, upcase the entire word
; | -        ; replace previous space seperator with a hyphen
; | [.,?!;:] ; literal punctuation
; | [Rr]     ; newline
; | [Ee]     ; end of input
; )*

; Compress a file into a word-list based format
(define (compress [in (current-input-port)])
  ; While processing, store words as a special struct
  ; mode is one of downcase, titlecase, upcase, specialcase
  (struct word (text mode) #:transparent)
  
  ; Store words we've already seen along with a count
  ; Later we're going to sort these so the most common are first
  (define words (make-hash))
  
  ; Helper to add a word to the dictionary and return the struct form
  (define (decode-word chunk)
    (define chunk/downcase (string-downcase chunk))
    (hash-update! words chunk/downcase (curry + 1) 1)
    (word chunk/downcase
          (cond
            [(equal? chunk chunk/downcase)                    'downcase]
            [(equal? chunk (string-upcase chunk/downcase))    'upcase]
            [(equal? chunk (string-titlecase chunk/downcase)) 'titlecase]
            [else                                             'specialcase])))
  
  ; Encode a 'chunk' which might actually turn into one or more tokens
  (define (decode-chunk chunk)
    (match chunk
      ; Break apart hyphens / punctuation / newlines
      [(regexp #px"(.*)([.,?!;:\\-\n\"'])(.*)" (list _ subchunk1 break subchunk2))
       (append (decode-chunk subchunk1)
               (list (if (equal? break "\n") 'newline (string->symbol break)))
               (decode-chunk subchunk2))]
      ; Process words
      [(regexp #px"([a-z]+|[A-Z][a-z]*|[A-Z]+)")
       (list (decode-word chunk))]
      ; Empty strings are empty lists (base case for breaking apart chunks)
      ["" '()]
      ; Anything else is a weird empty string, add it directly to the dictionary
      [any 
       (hash-update! words any (curry + 1) 1)
       (list (word any 'specialcase))]))
  
  ; Pull apart chunks, process them, put them back together
  (define chunks
    (apply append 
           (map decode-chunk 
                (string-split 
                 (string-replace (port->string in) "\r" "")
                 " "))))
  
  ; Replace word counts with an ascending count
  ; Write out the dictionary as we go
  (display (hash-count words)) 
  (display " ")
  (for ([i (in-naturals)]
        [word (in-list (map car (sort (hash->list words)
                                      (λ (a b) (> (cdr a) (cdr b))))))])
    (hash-set! words word i)
    (display word)
    (display " "))
  
  ; Replace each word with the numeric form
  ; Print out chunks as we go
  (for ([chunk (in-list chunks)])
    (match chunk
      [(word text mode)
       (display (hash-ref words text))
       (display 
        (case mode
          [(upcase)    "!"]
          [(titlecase) "^"]
          [else        ""]))]
      ['newline
       (display "R")]
      [any
       (display any)])
    (display " "))
  (displayln "E"))

; Compress in string mode
(define (compress/string str)
  (with-output-to-string
   (thunk
     (with-input-from-string str compress))))

; Decompress input compressed by the previous function
(define (decompress [in (current-input-port)])
  ; Read the dictionary; number of words followed by that many words
  (define words 
    (for/vector ([i (in-range (read in))])
      (~a (read in))))
  
  ; Read the rest of the file into a list of chunks
  ; Split on any whitespace, space vs tab vs newline are all the same
  (define chunks (string-split (port->string in)))
  (let/ec return
    (for/fold ([prefix ""]) ([chunk (in-list chunks)])
      (match chunk
        ; Numbers indicate words from the dictionary
        ; ^ means upcase the first letter
        ; ! means upcase the entire word
        [(regexp #px"(\\d+)(\\^|!|)" (list _ index flag))
         (define word (vector-ref words (string->number index)))
         (display prefix)
         (display
          (case flag
            [("^") (string-titlecase word)]
            [("!") (string-upcase word)]
            [else  word]))
         " "]
        ; Use a hyphen as a seperator rather than a space
        ["-" "-"]
        ; Punctuation literals
        [(or "." "," "?" "!" ";" ":" "\"" "'") (display chunk) " "]
        ; Newlines
        [(or "R" "r") "\n"]
        ; Early end of output (should generally be the last chunk)
        [(or "E" "e")
         (return)]))))

; Decompress in string mode
(define (decompress/string str)
  (with-output-to-string
   (thunk
     (with-input-from-string str decompress))))

(module+ test
  (require rackunit)
  
  (define input 
    "I do not like them in a house.
I do not like them with a mouse.
I do not like them here or there.
I do not like them anywhere.
I do not like green eggs and ham.
I do not like them, Sam-I-am.")
  
  (define compressed (compress/string input))
  (define output     (decompress/string compressed))
  
  (check-equal? input output))

(module+ main
  (match (current-command-line-arguments)
    [(vector "-c" in-file out-file)
     (printf "Compress ~a -> ~a\n" in-file out-file)
     (with-output-to-file out-file #:exists 'replace
       (thunk (with-input-from-file in-file compress)))]
    [(vector "-d" in-file out-file)
     (printf "Decompress ~a -> ~a\n" in-file out-file)
     (with-output-to-file out-file #:exists 'replace
       (thunk (with-input-from-file in-file decompress)))]
    [any
     (void)]))
