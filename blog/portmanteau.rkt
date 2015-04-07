#lang racket

(require graph)

(define current-minimum-overlap (make-parameter 3))
(define verbose-mode (make-parameter #f))
(define graph-mode (make-parameter #f))

(define (portmanteaus)
  (define words
    (for*/list ([raw-line (in-lines)]
                [line (in-value (string-trim (string-downcase raw-line)))]
                #:when (not (equal? "" line)))
      line))
  
  (for*/list ([left (in-list words)]
              [right (in-list words)]
              #:when (not (eq? left right))
              [portmanteau (in-value (portmanteau left right))]
              #:when portmanteau)
    portmanteau))

(define (portmanteau left right)
  (define maximum-overlap (- (min (string-length left) (string-length right)) 1))
  
  (for*/first ([overlap (in-range maximum-overlap (- (current-minimum-overlap) 1) -1)]
               #:when (equal? (substring left (- (string-length left) overlap))
                              (substring right 0 overlap)))
    (list left
          right
          (string-append 
           (substring left 0 (- (string-length left) overlap))
           right))))

(define paths
  (command-line
   #:program "portmanteau"
   #:once-each 
   [("--minimum-overlap")
    overlap
    "Specify the minimum necessary overlap (default = 3)"
    (cond
      [(string->number overlap) => current-minimum-overlap]
      [else (error '--minimum-overlap "must specify a number")])]
   #:once-any
   [("--verbose") 
    "Print in verbose mode (default = false)"
    (verbose-mode #t)]
   [("--graph")
    "Print out a dotfile"
    (graph-mode #t)]
   #:args paths
   
   paths))

(when (null? paths)
  (set! paths '("-")))

(for ([path (in-list paths)])
  (define results
    (cond 
      [(equal? path "-")
       (portmanteaus)]
      [else
       (with-input-from-file path portmanteaus)]))
  
  (define g (unweighted-graph/directed '()))
    
  (for ([result (in-list results)])
    (match-define (list left right portmanteau) result)
    (cond
      [(verbose-mode)
       (printf "~a + ~a = ~a\n" left right portmanteau)]
      [(graph-mode)
       (add-edge! g (~a "\"" left "\"") (~a "\"" right "\""))]
      [else
       (displayln portmanteau)]))
  
  (when (graph-mode)
    (displayln (graphviz g))))
