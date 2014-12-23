#lang racket

(require images/flomap
         "util.rkt"
         "recolor-sort.rkt"
         "recolor-swap.rkt"
         "recolor-fill.rkt")

(define file-list
  (filter (curry regexp-match #px"\\.png$") (directory-list "input")))

(for* ([(name function) 
        (in-hash 
         (hash
          'sort recolor/sort
          'swap-100 recolor/swap
          'swap-1k (λ (l r) (recolor/swap l r #:threshold 1000))
          'swap-1k-average (λ (l r) (recolor/swap l r #:threshold 1000 #:error-function error/rgb-distance/average))
          'fill recolor/fill
          ))]
       [left (in-list file-list)]
       [right (in-list file-list #;(cons 'all-rgb file-list))]
       #:when (not (equal? left right)))
  
  (make-directory* "output")
  
  (define output-filename
    (build-path 
     "output"
     (~a (regexp-replace #px"\\.png$" (path->string left) "")
         "_"
         (if (eq? right 'all-rgb)
             right
             (regexp-replace #px"\\.png$" (path->string right) ""))
         "_"
         name
         ".png")))
  
  (cond
    [(file-exists? output-filename)
     (printf "~a exists, skipped\n" output-filename)]
    [else
     (printf "~a ...\n" output-filename)
     
     (define-values (result* cpu-time real-time gc-time)
       (cond
         [(eq? right 'all-rgb)
          (let* ([left (load-flomap (build-path "input" left))]
                 [right 
                  (make-allrgb
                   (flomap-width left) (flomap-height left) 
                   #:channels
                   (flomap-components left))])
            (time-apply function (list left right)))]
         [else
          (time-apply
           function 
           (list (build-path "input" left) 
                 (build-path "input" right)))]))
     
     (with-output-to-file "output/timing.txt"
       #:exists 'append
       (λ ()
         (displayln 
          (string-join (map ~a (list left right name cpu-time real-time gc-time)) 
                       ","))))
     
     (send (first result*) save-file output-filename 'png)]))
         
         
         