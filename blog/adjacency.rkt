#lang racket

; hashset adt, a hash with sets for values

(define (hashset-add! hash key val)
  (hash-set! hash key (set-add (hash-ref hash key (set)) val)))

(define (hashset-has? hash key val)
  (set-member? (hash-ref hash key (set)) val))

; Convert a string containing a list of nubmers into those numbers
(define (number-string->list str)
  (map string->number (string-split (string-trim str))))

; Given a properly formatted list of edges, print an adjaceny matrix
(define (adjacency [in/str (current-input-port)])
  ; Read from either a string or a port
  (define in (if (string? in/str) (open-input-string in/str) in/str))
  
  ; Read header: <n = number of nodes> <m = number of lines defining edges>
  (define node-count (read in))
  (define line-count (read in))
  
  ; Read edges, each line is: <edge start>+ "->" <edge end> "\n"
  ; Skip empty lines
  (define edges (make-hash))
  (for ([line (in-lines in)] #:when (not (equal? "" (string-trim line))))
    (define parts (string-split line "->"))
    ; Add all edges, there can be one or more of both from or to nodes
    (for* ([from (in-list (number-string->list (first parts)))]
           [to   (in-list (number-string->list (second parts)))])
      (hashset-add! edges from to)))
  
  ; Print out the adjacency matrix 
  (for ([i (in-range node-count)])
    (for ([j (in-range node-count)])
      (display (if (hashset-has? edges i j) 1 0)))
    (newline)))

(module+ test
  (require rackunit)
  
  (define in
    "5 4
0 -> 1 3
1 -> 2
2 -> 4
3 -> 4
")
  (define out 
    "01010
00100
00001
00001
00000
")
  
  (check-equal? 
   (with-output-to-string (thunk (adjacency in)))
   out))
