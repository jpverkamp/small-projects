#lang racket

(require "vector-math.rkt"
         "stop-word-frequency.rkt"
         "n-gram-frequency.rkt"
         "word-rank.rkt"
         "word-lengths.rkt")

(define-syntax flushed-printf
  (syntax-rules ()
    [(_ fmt args ...)
     (let ()
       (printf fmt args ...)
       (flush-output))]))

(define current-target (make-parameter #f))
(define current-author-regexp (make-parameter #f))

(define functions
  (list #;stop-word-frequency
        #;n-gram-frequency
        word-rank
        #;word-lengths
        ))
  
(define (load-file file functions)
  (for/hasheq ([function (in-list functions)])
    (values function (with-input-from-file file function))))

(define (get-author file)
  (define match (regexp-match (current-author-regexp) file))
  (and match (second match)))

(command-line
 #:program "jk"
 #:once-each
 [("-t" "--target")
  target
  "Text file to analyze"
  (current-target target)]
 [("-a" "--author")
  author-regexp
  "Regular expression matching author in filenames"
  (current-author-regexp (regexp author-regexp))]
 #:args files
 
 (unless (current-target)
   (error "Must specify a target file"))
 
 ; Load target
 (flushed-printf "Loading: ~a\n" (current-target))
 (define target
   (load-file (current-target) functions))
 
 ; Load library
 (define library
   (for/hash ([file (in-list files)])
     (flushed-printf "Loading: ~a\n" file)
     (values file (load-file file functions))))
 
 ; Get a list of authors
 (define authors
   (sort 
    (set->list
     (for/set ([file (in-list files)])
       (get-author file)))
    string<?))
 
 (flushed-printf "\n--- Authors --\n")
 (for ([author (in-list authors)])
   (flushed-printf "~a\n" author))
 
 ; Calculate all of the similarities
 (for ([function (in-list functions)])
   ; Scores for individual books
   (flushed-printf "\n--- ~a by book ---\n" function)
   
   (define scores
     (sort
      (for/list ([file (in-list files)])
        (list
         file
         (cosine-similarity 
          (hash-ref target function)
          (hash-ref (hash-ref library file) function))))
      (lambda (a b) (> (second a) (second b)))))
   
   (for ([each (in-list scores)])
     (flushed-printf "~a\t~a\n" (second each) (first each)))
   
   ; Scores for authors
   (flushed-printf "\n--- ~a by author ---\n" function)
   
   (define author-scores
     (sort
      (for/list ([author (in-list authors)])
        (define count (make-parameter 0))
        (define score
          (for/sum ([file (in-list files)]
                    #:when (equal? author (get-author file)))
            (count (add1 (count)))
            (cosine-similarity 
             (hash-ref target function)
             (hash-ref (hash-ref library file) function))))
        (list author (* 1.0 (/ score (count)))))
      (lambda (a b) (> (second a) (second b)))))
        
   (for ([each (in-list author-scores)])
     (flushed-printf "~a\t~a\n" (second each) (first each)))
   )
 )