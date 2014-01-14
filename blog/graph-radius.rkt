#lang racket

(require graph)

; Read a graph in the specified format
(define (read-graph [str/in (current-input-port)])
  (define in (if (string? str/in) (open-input-string str/in) in))
  (define node-count (read in))
  (unweighted-graph/directed
   (for*/list ([i (in-range node-count)]
               [j (in-range node-count)]
               #:when (= 1 (read in)))
     (list i j))))

; Calculate the distance between n0 and n1 in the graph g
(define (distance g n0 n1)
  ; Start with 0 units to the origin
  (let loop ([distances (hash n0 0)])
    (cond
      ; If we ever have a distance to the target, just return that directly
      [(hash-ref distances n1 #f) => identity]
      ; Otherwise, try to extend each node already in the graph
      ; It would be more efficient to only expand each node once, but so it goes
      [else
       (define next-distances (hash-copy distances))
       (for* ([(node distance) (in-hash distances)]
              [neighbor (in-neighbors g node)]
              #:unless (hash-has-key? distances neighbor))
         (hash-set! next-distances neighbor (+ distance 1)))
       
       ; If we have any new nodes, keep looking; otherwise we aren't connected
       (if (= (hash-count distances) (hash-count next-distances))
           #f
           (loop next-distances))])))

; The eccentricity of a node is the distance to the furthest other node
(define (eccentricity g n)
  (apply max (map (curry distance g n) (in-vertices g))))

; The radius of a graph is the minimum eccentricity
(define (graph-radius g)
  (apply min (map (curry eccentricity g) (in-vertices g))))
  
; The diameter of a graph is maximum eccentricity
(define (graph-diameter g)
  (apply max (map (curry eccentricity g) (in-vertices g))))

(module+ test
  (require rackunit)
  
  (define petersen-graph (read-graph "
10
0 1 0 0 1 1 0 0 0 0
1 0 1 0 0 0 1 0 0 0
0 1 0 1 0 0 0 1 0 0
0 0 1 0 1 0 0 0 1 0
1 0 0 1 0 0 0 0 0 1
1 0 0 0 0 0 0 1 1 0
0 1 0 0 0 0 0 0 1 1
0 0 1 0 0 1 0 0 0 1
0 0 0 1 0 1 1 0 0 0
0 0 0 0 1 0 1 1 0 0
"))
  (check-equal? (graph-radius petersen-graph) 2)
  (check-equal? (graph-diameter petersen-graph) 2)
  
  (define butterfly-graph (read-graph "
5
0 1 1 0 0
1 0 1 0 0
1 1 0 1 1
0 0 1 0 1
0 0 1 1 0
"))
  (check-equal? (graph-radius butterfly-graph) 1)
  (check-equal? (graph-diameter butterfly-graph) 2)
  
  (define sample-inputs/outputs (read-graph "
10
0 1 0 0 1 1 0 0 0 0
1 0 1 0 0 0 1 0 0 0
0 1 0 1 0 0 0 1 0 0
0 0 1 0 1 0 0 0 1 0
1 0 0 1 0 0 0 0 0 1
1 0 0 0 0 0 0 1 1 0
0 1 0 0 0 0 0 0 1 1
0 0 1 0 0 1 0 0 0 1
0 0 0 1 0 1 1 0 0 0
0 0 0 0 1 0 1 1 0 0
"))
  (check-equal? (graph-radius sample-inputs/outputs) 2)
  (check-equal? (graph-diameter sample-inputs/outputs) 2)
  
  (define nauru-graph (read-graph "
24
0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
0 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0
0 0 0 0 0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0
0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0
0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0
0 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0
0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1
0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 1 0
1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0
0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1
0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0
"))
  (check-equal? (graph-radius nauru-graph) 4)
  (check-equal? (graph-diameter nauru-graph) 4)
   
  (define desargues-graph (read-graph "
20
0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 0 0 0 0
0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0
0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0
0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1
0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 1 0
0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 1
0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1
0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0
0 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0
0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0
0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0
0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0
"))
  (check-equal? (graph-radius desargues-graph) 5)
  (check-equal? (graph-diameter desargues-graph) 5)

  )
