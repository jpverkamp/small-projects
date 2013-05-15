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
; iarzkcfjotudepgbswxqvmhlyn (474)

; load a word list to score from
;(define fn "word-list-short.txt")
(define fn "dict.txt")
(define word-list
  (with-input-from-file fn
    (lambda () (for/vector ([word (in-lines)]) (string-downcase word)))))

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
  (for/sum ([word (in-vector word-list)])
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

; calculate a compatibility matrix
(define (compatible-matrix)
  ; test if two words can both be alphabetical
  ; incompatible iff *a*b* in w1 and *b*a* in w2
  (define (compatible? w1 w2)
    (define (@1 i) (string-ref w1 i))
    (define (@2 i) (string-ref w2 i))
    (for*/fold ([compatible #t])
      ([i1 (in-range (string-length w1))]
       [j1 (in-range i1 (string-length w1))]
       [i2 (in-range (string-length w2))]
       [j2 (in-range i2 (string-length w2))])
      (and compatible
           (or ; ignore *a*a* in either word
            (eq? (@1 i1) (@1 j1))
            (eq? (@2 i2) (@2 j2))
            ; fail on *a*b* / *b*a*
            (not (and (eq? (@1 i1) (@2 j2))
                      (eq? (@1 j1) (@2 i2))))))))
  
  ; if the file doesn't exist, generate it
  (for/vector ([i (in-range (vector-length word-list))])
    (printf "working: ~a/~a: ~a\n" 
            (+ i 1)
            (vector-length word-list)
            (vector-ref word-list i))
    (for/vector ([j (in-range (vector-length word-list))])
      (compatible? (vector-ref word-list i)
                   (vector-ref word-list j)))))  

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
  (for* ([word (in-vector word-list)]
         [i (in-range (string-length word))]
         [j (in-range i (string-length word))])
    (++ (- (char->integer (string-ref word i)) 97)
        (- (char->integer (string-ref word j)) 97)))
  
  ; reset diagonal
  (for ([i (in-range 26)])
    (vector-set! (vector-ref m i) i 0))
  
  m)

; simple recursive search on the word lists
(define (solve/search)
  ; test if two words can both be alphabetical
  ; incompatible iff *a*b* in w1 and *b*a* in w2
  (define (compatible? w1 w2)
    (define (@1 i) (string-ref w1 i))
    (define (@2 i) (string-ref w2 i))
    (for*/fold ([compatible #t])
               ([i1 (in-range (string-length w1))]
                [j1 (in-range i1 (string-length w1))]
                [i2 (in-range (string-length w2))]
                [j2 (in-range i2 (string-length w2))])
      (and compatible
           (or ; ignore *a*a* in either word
               (eq? (@1 i1) (@1 j1))
               (eq? (@2 i2) (@2 j2))
               ; fail on *a*b* / *b*a*
               (not (and (eq? (@1 i1) (@2 j2))
                         (eq? (@1 j1) (@2 i2))))))))
  
  (define best-block #f)
  (define best-size 0)
  (define (search current-block current-size remaining-words)
    (for ([word (in-list remaining-words)])
      ; report new larger blocks
      (when (> current-size best-size)
        (printf "new best block: ~a\n" current-size)
        (set! best-block current-block)
        (set! best-size current-size))
      
      ; the word works, search with it
      (when (andmap (lambda (block-word) 
                      (compatible? block-word word))
                  current-block)
        (search (cons word current-block) 
                (+ 1 current-size)
                (remove word remaining-words)))
      
      ; either way, try without it
      (search current-block 
              current-size
              (remove word remaining-words))))
  
  (search '() 0 (vector->list word-list)))

; solve using the bron-kerbosch method to calculate the maximal clique
(define (solve/graph/bron-kerbosch)
  (define cm (compatible-matrix))
  (define (cm? i j)
    (vector-ref (vector-ref cm i) j))
  
  ; find the set of neighbors of a vertex v
  (define (neighbors v)
    (for/set ([i (in-range (vector-length word-list))] 
              #:when (and (not (= v i)) (cm? v i))) 
             i))
  
  ; use the Bron-Kerbosch algorithm to find all maximal cliques
  ; remember the largest
  ; http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
  ; this is hacky :(
  (define (set-first s) (car (set->list s)))
  
  (define best-clique #f)
  (define best-size 0)
  
  (define (bron-kerbosch r p x)
    (if (and (set-empty? p) (set-empty? x))
        ; found a new maximal clique, test if it's bigger
        (when (>= (set-count r) best-size)
          (printf "maximal clique found (~s items)\n~s\n" (set-count r) r)
          (set! best-clique r)
          (set! best-size (set-count r)))
        ; otherwise loop
        (let loop ([p p] [x x])
          (when (not (set-empty? p))
            (define v (set-first p))
            (define nv (neighbors v))
            (bron-kerbosch (set-union r (set v))
                           (set-intersect p nv)
                           (set-intersect p nv))
            (loop (set-remove p v) (set-union x (set v)))))))
  
  (define (bron-kerbosch2 r p x)
    (cond
      [(and (set-empty? p) (set-empty? x))
       ; found a new maximal clique, test if it's bigger
       (when (>= (set-count r) best-size)
         (printf "maximal clique found (~s items)\n~s\n" (set-count r) r)
         (set! best-clique r)
         (set! best-size (set-count r)))]
        ; otherwise choose a pivot then loop loop
      [else
       (define pivot (car (shuffle (set->list (set-union p x)))))
       (define npivot (neighbors pivot))
       (let loop ([vs (set-subtract p (neighbors pivot))] [p p] [x x])
         (when (not (set-empty? vs))
           (define v (set-first vs))
           (define nv (neighbors v))
           
           (bron-kerbosch2 (set-union r (set v))
                           (set-intersect p nv)
                           (set-intersect p nv))
           
           (loop (set-remove vs v)
                 (set-remove p v)
                 (set-union x (set v)))))]))
  
  (bron-kerbosch2 (set) (list->set (range (vector-length word-list))) (set))
  
  best-clique)
  

(solve/hill-climbing)

