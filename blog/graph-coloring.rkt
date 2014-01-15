#lang racket

(require graph)

; Given a string or input port, read a graph
; First line is number of following lines
; The rest of the lines have a node id than one or more ids of that node's neighbors
(define (read-graph [str/in (current-input-port)])
  (define in (if (string? str/in) (open-input-string str/in) str/in))
  (define node-count (read in))
  
  (define g (unweighted-graph/undirected '()))
  
  (for* ([i (in-range node-count)]
         [line (in-lines in)])
    (define nums (map string->number (string-split line)))
    (when (> (length nums) 1)
      (for ([n (in-list (rest nums))])
        (add-edge! g (first nums) n))))
  
  g)

; Wrap in-neighbors to give a list
(define (neighbors g n) (sequence->list (in-neighbors g n)))

; Assign a color given a graph, color hash, and node
(define (assign-first-color! g cs n)
  (for/first ([i (in-naturals)]
              #:unless 
              (member i (map (λ (n) (hash-ref cs n #f))
                               (neighbors g n))))
      (hash-set! cs n i)))

; Basic greedy coloring: color each node in turn with the first available color
(define (greedy-coloring g [node-order (in-vertices g)])
  (define colors (make-hash))
  
  ; For each node, try each color
  ; for/first will bail as soon as it is execute once
  (for ([n (in-list node-order)])
    (assign-first-color! g colors n))
  
  colors)

; Try a bunch of random colorings, keeping the best
(define (random-coloring g #:iterations [iterations 1e6])
  
  (define-values (coloring count)
    (for/fold ([best-coloring #f] [best-count +inf.0])
              ([i (in-range iterations)])
      (define new-coloring (greedy-coloring g (shuffle (in-vertices g))))
      (define new-count (set-count (list->set (hash-values new-coloring))))
      
      (if (< new-count best-count)
          (values new-coloring new-count)
          (values best-coloring best-count))))
  
  coloring)

; Use a Brélaz coloring: 
;   Choose the vertex with the most colored neighbors,
;   breaking ties by most uncolored neighbors
(define (brélaz-coloring g)
  (define colors (make-hash))
  
  ; Used to break ties as mentioned above
  (define (count-colored-neighbors n)
    (length (filter (curry hash-has-key? colors) (neighbors g n))))
  
  (define (count-uncolored-neighbors n)
    (length (filter (negate (curry hash-has-key? colors)) (neighbors g n))))
  
  (define graph-size (length (in-vertices g)))
  (define (brélaz-number n)
    (+ (* (count-colored-neighbors n) graph-size)
       (count-uncolored-neighbors n)))
  
  ; Each time, color the node with the highest current brélaz-number (see above)
  (for ([i (in-range graph-size)])
    (assign-first-color! 
     g 
     colors 
     (first
      (sort
       (filter (negate (curry hash-has-key? colors)) (in-vertices g))
       (λ (n1 n2) (> (brélaz-number n1) (brélaz-number n2)))))))
  
  colors)

(define brelaz-coloring brélaz-coloring)

; Try every possible coloring (this is crazy slow)
(define (perfect-coloring g)
  ; Return all permutations of a given list as a sequence
  (define (in-permutations ls)
    (local-require racket/generator)
    (in-generator
     (let loop ([ls ls] [acc '()])
       (cond
         [(null? ls) 
          (yield acc)]
         [else
          (for ([a (in-list ls)])
            (loop (remove a ls) (cons a acc)))]))))
  
  ; Try each coloring in turn
  (define-values (coloring count)
    (for/fold ([best-coloring #f] [best-count +inf.0])
              ([coloring-order (in-permutations (in-vertices g))])

      (define new-coloring (greedy-coloring g coloring-order))
      (define new-count (set-count (list->set (hash-values new-coloring))))
      
      (if (< new-count best-count)
          (values new-coloring new-count)
          (values best-coloring best-count))))
  
  coloring)

; Calculate the chromatic number of a graph, potentially given a coloring function
(define (chromatic-number g #:coloring-function [coloring perfect-coloring])
  (add1 (apply max (hash-values (coloring g)))))

; Output a graph in graphviz / dot format, potentially with coloring
(define (graphviz g
                  #:coloring-function [coloring #f] 
                  #:horizontal [horizontal #f]
                  #:save-as-png [save-as-png #f])
  ; Generate the dot file
  (define dot-file
    (with-output-to-string
      (thunk
        (printf "graph G {\n")
        
        ; Prefer horizontal layout to vertical
        (when horizontal
          (printf "\trankdir=LR;\n"))
        
        ; Color nodes using evenly spaced HSV colors
        (when coloring
          (define colors (coloring g))
          (define color-count (add1 (apply max (hash-values colors))))
          
          (for ([(node color) (in-hash colors)])
            (printf "\t~a [color=\"~a 1.0 1.0\"];\n"
                    node 
                    (~a #:max-width 5 (exact->inexact (/ color color-count))))))
        
        ; Write out all edges (directional, so only if sorted)
        (for ([edge (in-edges g)])
          (when (< (first edge) (second edge))
            (printf "\t~a -- ~a;\n" (first edge) (second edge))))
        
        (printf "}\n"))))
  
  ; Either save via buffer file or just return the dot file text
  (cond
    [save-as-png
     (with-output-to-file #:exists 'replace "output.dot" (thunk (display dot-file)))
     (system (format "dot output.dot -Kneato -Tpng -s0.5 -o ~a" save-as-png))]
    [else
     dot-file]))
