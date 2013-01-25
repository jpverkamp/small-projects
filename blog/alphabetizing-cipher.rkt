#lang racket

; found solutions: 

; solve/monte-carlo
; iacxvdreofugkphbtwzsjlmqyn (440)
; hcawtqpemyrkbnfduvzsilgjxo (385)
; hbrxucsenftdmogavwzqipjlyk (425)
; idpwuamrkbqfelgczvyojthnxs (448)

; solve/hill-climbing
; jdrxvasuobtegphcfwzqknimyl (470)
; ibsxvaruoetfdpgclwzqmnhkyj (470)
; kjpxvuqioesdfngcawzrtmhlyb (473)
; iarzkcfjotudepgbswxqvmhlyn (474)

; load a word list to score from
(define fn "word-list.txt")
(define word-list
  (with-input-from-file fn
    (lambda () (for/list ([word (in-lines)]) word))))

; create an encoding function from a key
(define (make-cipher key)
  (lambda (plaintext)
    (build-string
     (string-length plaintext)
     (lambda (i)
       (string-ref key (- (char->integer (string-ref plaintext i)) 97))))))

; test if a word is sorted
(define (word-sorted? word)
  (andmap 
   (lambda (i) (char<=? (string-ref word i) (string-ref word (+ i 1))))
   (range (- (string-length word) 1))))

; score a cipher
(define (score-cipher cipher)
  (for/sum ([word (in-list word-list)])
    (if (word-sorted? (cipher word)) 1 0)))

; score a key
(define (score-key key)
  (score-cipher (make-cipher key)))

; generate a random key
(define (random-key)
  (list->string (shuffle (string->list "abcdefghijklmnopqrstuvwxyz"))))

; swap two letters in a string
(define (string-swap! str i j)
  (define c (string-ref str i))
  (string-set! str i (string-ref str j))
  (string-set! str j c))

; functional string swap
(define (string-swap str i j)
  (build-string
   (string-length str)
   (lambda (x)
     (cond
       [(= x i) (string-ref str j)]
       [(= x j) (string-ref str i)]
       [else (string-ref str x)]))))

; solve via random swapping
(define (solve/monte-carlo [guess (random-key)]
                           [score -inf.0]
                           [timeout 10]
                           [timer (current-seconds)])
  
  ; generate a new guess by swapping up to 10 pairs
  (define new-guess (string-copy guess))
  (for ([i (in-range (+ (random 10) 1))])
    (string-swap! new-guess (random 26) (random 26)))
  (define new-score (score-key new-guess))
  
  ; if we have a new best, report it
  (cond
    [(> (- (current-seconds) timer) timeout)
     (printf "timeout\n")
     (solve/monte-carlo (random-key) -inf.0 timeout (current-seconds))]
    [(> new-score score)
     (printf "~a (~a)\n" new-guess new-score)
     (solve/monte-carlo new-guess new-score timeout (current-seconds))]
    [else
     (solve/monte-carlo guess score timeout timer)]))

; solve via direct hill climbing
(define (solve/hill-climbing [guess (random-key)]
                             [score -inf.0]
                             [overall-guess guess]
                             [overall-score score])
  
  ; try every possible single swap
  (define-values (new-guess new-score)
    (for*/fold ([guess guess]
                [score score])
               ([i (in-range 26)]
                [j (in-range i 26)])
      (define new-guess (string-swap guess i j))
      (define new-score (score-key new-guess))
      (if (> new-score score)
          (values new-guess new-score)
          (values guess score))))

  ; update the overall best (will actually print next round)
  (define-values (new-overall-guess new-overall-score)
    (if (>= new-score overall-score)
        (values new-guess new-score)
        (values overall-guess overall-score)))
  
  ; print out local best values and best overall
  (cond
    [(equal? guess new-guess)
     (printf "local maximum, shuffling\n")
     (for ([i (in-range (+ (random 6) 4))])
       (string-swap! guess (random 26) (random 26)))
     (define new-score (score-key new-guess))
     (printf "~a (~a)  \toverall: ~a (~a)\n" new-guess new-score overall-guess overall-score) 
     (solve/hill-climbing new-guess new-score new-overall-guess new-overall-score)]
    [else
     (printf "~a (~a)  \toverall: ~a (~a)\n" new-guess new-score overall-guess overall-score) 
     (solve/hill-climbing new-guess new-score new-overall-guess new-overall-score)]))

; solve using matrix math
; TODO: unfinished
(define (solve/matrix-math)
  ; count how many times each letter should be after each letter
  (define m
    (for/vector ([i (in-range 26)])
      (for/vector ([i (in-range 26)])
        0)))
  
  ; increment a cell in that matrix
  (define (@ i j)
    (vector-ref (vector-ref m i) j))
  (define (++ i j)
    (vector-set! (vector-ref m i) j (+ 1 (@ i j))))
  
  ; add all of the words
  (for* ([word (in-list word-list)]
         [i (in-range (string-length word))]
         [j (in-range i (string-length word))])
    (++ (- (char->integer (string-ref word i)) 97)
        (- (char->integer (string-ref word j)) 97)))
  
  ; reset diagonal
  (for ([i (in-range 26)])
    (vector-set! (vector-ref m i) i 0))
  
  m)