#lang racket

(require "dictionary.rkt")
(require "queue.rkt")

; create a string by swapping one letter
(define (string-set s i c)
  (let ([s (string-copy s)])
    (string-set! s i c)
    s))

; calculate the difference between two strings
(define (string-diff s1 s2)
  (+ (random)
     (for/sum ([c1 s1] [c2 s2]) 
       (abs (- (char->integer c1) (char->integer c2))))))

; find the path between two words, changing one letters at a time
; use call/cc to bail out when we find an answer
(define (direct-doublet dict src dst)
  (call/cc 
   (? (exit) 
     (let ([src (string-upcase src)]
           [dst (string-upcase dst)])
       ; loop down possible solutions
       (let loop ([current src] [words (list src)])
         ; when we find one, bail out entirely
         (if (equal? current dst)
             (exit (reverse words))
             ; try all possible values
             (for*/list ([i (string-length src)]
                         [c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"])
               (let ([next (string-set current i c)])
                 (when (and (not (member next words))
                            (contains? dict next))
                   (loop next (cons next words))))))))
     (exit #f))))

; find the path between two words, changing one letters at a time
; use call/cc to bail out when we find an answer
(define (doublet dict src dst)
  (call/cc 
   (? (exit) 
     (let ([src (string-upcase src)]
           [dst (string-upcase dst)])
       ; loop recursively
       (let loop ([current src] [words (list src)])
         ; bail when we find any solution
         (if (equal? current dst)
             (exit (reverse words))
             ; find all of the next steps
             (let ([nexts 
                    (for*/list ([i (string-length src)]
                                [c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
                                #:when 
                                (let ([next (string-set current i c)])
                                  (and (not (member next words))
                                       (contains? dict next))))      
                      (string-set current i c))])
               ; sort by distance to the final solution, recur in that order
               (for ([next (sort nexts (? (w1 w2) (< (string-diff w1 dst)
                                                     (string-diff w2 dst))))])
                 (loop next (cons next words)))))))
     (exit #f))))

; find the path between two words, using a queue
; values in the queue are (current, path)
(define (breadth-doublet dict src dst)
  (call/cc 
   (? (exit) 
     (let ([src (string-upcase src)]
           [dst (string-upcase dst)]
           [q (make-queue)])
       ; start with just the initial solution
       (queue-push! q (list src (list src)))
       ; loop as long as the queue isn't empty, popping the first each time
       (let loop ()
         (when (not (queue-empty? q))
           (let* ([next (queue-pop! q)]
                  [curr (car next)]
                  [wrds (cadr next)])
             ; if we find a solution, it's optimal
             (if (equal? curr dst)
                 (exit (reverse wrds))
                 (begin
                   ; find all next steps, push them onto the queue
                   (for* ([i (string-length src)]
                          [c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"])
                     (let ([next (string-set curr i c)])
                       (when (and (not (member next wrds))
                                  (contains? dict next))
                         (queue-push! q (list next (cons next wrds))))))
                   (loop)))))))
     (exit #f))))