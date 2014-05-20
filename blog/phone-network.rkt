#lang racket

(require graph)

; Read a phone network as a weighted undirect graph, form {from} {to} {weight}
(define (read-network [in (current-input-port)])
  (define g (weighted-graph/directed '()))
  (for ([line (in-lines in)]
        #:break (equal? line ""))
    (match-define (list from to weight) (string-split line))
    (add-edge! g from to (string->number weight))
    (add-edge! g to from (string->number weight)))
  
  g)

; Read a sequential list of calls formatted as {from} {to}
(define (read-calls [in (current-input-port)])
  (for/list ([line (in-lines in)])
    (string-split line)))

; Specialization of the graph library to find a single path using dijkstra's
(define (dijkstra-path graph from to)
  (define-values (_ preds)
    (dijkstra graph from))
  
  (let loop ([to to] [path '()])
    (cond
      [(equal? from to) 
       (cons to path)]
      [(hash-ref preds to #f)
       => (λ (next)
            (loop next (cons to path)))]
      [else
       #f])))

; Each step of a solution will have
; - the current graph (before routing)
; - the call being made
; - the found route (or #f)
(struct step (graph call route) #:transparent)

; Given a phone network and sequence of calls, place as many as possible
(define (solve [in (current-input-port)])
  (define network (read-network in))
  (define calls (read-calls in))
  
  ; Try to route each call in turn
  ; Take the shortest path possible using dijkstra's algorithm for routing
  (for/list ([call (in-list calls)])
    (match-define (list from to) call)
    (cond
      ; We can find a path; write it and update the network
      [(dijkstra-path network from to)
       => (λ (path)
            (begin0
              (step (graph-copy network) call path)
              (let loop ([path path])
                (match path
                  ; As long as there are at least two nodes:
                  ; - Calculate the new edge weight
                  ; - Remove the current edge (no way to directly update)
                  ; - If new weight is not zero, add the new edges
                  [(list-rest from to rest)
                   (define weight (- (edge-weight network from to) 1))
                   (remove-edge! network from to)
                   (remove-edge! network to from)
                   (when (> weight 0)
                     (add-edge! network from to weight)
                     (add-edge! network to from weight))
                   (loop rest)]
                  [any (void)]))))]
      ; No path; print failure and leave the network
      [else
       (step (graph-copy network) call #f)])))

; Write a solution to a directory
(define (write-solution steps output-directory)
  ; Make sure the output directory exists (files will be overwritten)
  (when (not (directory-exists? output-directory))
    (make-directory output-directory))
  
  ; First generate a summary file
  (with-output-to-file (build-path output-directory "summary.txt")
    (thunk
      (for ([i (in-naturals 1)]
            [each (in-list steps)])
        
        (match-define (step network call path) each)
        (match-define (list from to) call)
        
        (printf "~a -> ~a ... ~a\n"
                from
                to
                (or path "failed")))))
  
  ; Generate a graph for each step
  (for ([i (in-naturals 1)]
        [each (in-list steps)])
    
    (match-define (step network call path) each)
    
    ; Generate a coloring with distinct colors for path
    (define colors
      (for/hash ([node (in-vertices network)])
        (values node (cond
                       [(equal? node (first path)) 1]
                       [(equal? node (last path))  2]
                       [(member node path)         3]
                       [else                       0]))))
    
    ; Paramaterize each step filename so we can make dots and images
    (define (filename ext)
      (format "~a.~a" 
              (~a i #:min-width 2 #:align 'right #:pad-string "0")
              ext))
    
    ; Write the dot file
    (with-output-to-file (build-path output-directory (filename "dot"))
      #:exists 'replace
      (thunk
        (display (graphviz network #:colors colors))))
    
    ; Use the dot file to generate an image
    (system (format "neato -Tpng ~a > ~a" 
                    (build-path output-directory (filename "dot"))
                    (build-path output-directory (filename "png"))))))
