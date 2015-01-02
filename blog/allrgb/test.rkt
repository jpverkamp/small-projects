#lang racket

(require "colors.rkt"
         "pixel-orders.rkt"
         "render.rkt")

(make-directory* "test-output")

(define width 100)
(define height 100)

(when (= 2 (vector-length (current-command-line-arguments)))
  (set! width (string->number (vector-ref (current-command-line-arguments) 0)))
  (set! height (string->number (vector-ref (current-command-line-arguments) 1))))

(debug:render #t)

(for* ([(rgb-name rgb-producer) (in-rgb-producers)]
       [(order-name order-producer) (in-order-producers)])
  (make-directory* (build-path "test-output" (~a width "x" height)))
  
  (define filename 
    (build-path 
     "test-output" 
     (~a width "x" height)
     (~a order-name "_" 
         rgb-name "_"
         width "x" height
         ".png")))
  
  (newline)
  (cond
    [(file-exists? filename)
     (printf "~a exists, skipping\n" filename)
     (flush-output)]
    [else
     (printf "~a\n" filename)
     (flush-output)
  
     (define image 
       (time
        (begin0
          (render width height rgb-producer order-producer)
          (newline))))
     
     (send image save-file filename 'png)]))