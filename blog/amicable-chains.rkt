#lang racket

(require graph
         math/number-theory
         memoize)

(define (snoc x ls) (reverse (cons x (reverse ls))))

; Calculate sum of proper divisors (proper, thus subtracting i)
(define/memo (sum-of-divisors n)
  (- (apply + (divisors n)) n))

; A number is perfect if its divisors sum to itself
(define (perfect? n)
  (= n (sum-of-divisors n)))

; Two numbers are amicable if each numbers's sum of divisors is the other
(define (amicable? m n)
  (and (= m (sum-of-divisors n))
       (= n (sum-of-divisors m))))

; An amicable chain is a sequence of numbers where each's sum of divisors is the next
; Ignore chains that leave the given bounds
(define (amicable-chain n [bound +inf.0])
  (let/ec return
    (let loop ([x (sum-of-divisors n)] [prev '()])
      (cond
        [(= x n)
         (cons x prev)]
        [(or (> x bound) (= x 1) (member x prev))
         (return #f)]
        [else
         (loop (sum-of-divisors x)
               (cons x prev))]))))

; Create a graph of all amicable chains within the bounds
(define (amicable-chains-graph bound)
  (define graph (unweighted-graph/directed '()))
  (define colors (make-hash))
  
  (for ([i (in-range 2 bound)] #:unless (has-vertex? graph i))
    (cond 
      [(amicable-chain i bound) 
       => (λ (chain)
            (displayln chain)
            (for ([from (in-list chain)] 
                  [to (in-list (snoc (car chain) (cdr chain)))])
              (hash-set! colors from
                         (case (length chain)
                           [(1) 0] [(2) 1] [else 2]))
              (add-directed-edge! graph from to)))]))
  
  (values graph colors))

; Wrapper to both save the dot file and generate a nice image
(define (save-graph graph colors filename)
  (define dot-filename (format "~a.dot" filename))
  (with-output-to-file dot-filename
    #:exists 'replace
    (thunk 
      (display (graphviz graph #:colors colors))))
  (system (format "neato -Tpng ~a > ~a" dot-filename filename)))
  

#;(begin
  (define-values (graph colors)
    (time
     (amicable-chains-graph 1000000)))

  (save-graph graph colors "sample.png"))
