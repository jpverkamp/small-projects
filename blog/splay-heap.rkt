#lang racket

(provide (rename-out [new-heap make-heap])
         empty?
         first
         rest
         heapsort)

; splay heap nodes
(define-struct node (value left right) #:transparent)

(define empty-node (node (void) #f #f))
(define (empty-node? node) (and (node? node) (void? (node-value node))))

; the entire heap, store comparator
(define-struct heap (root <) #:transparent)

; create an empty heap
; renamed as make-heap
(define (new-heap <)
  (make-heap empty-node <))

; test if a heap is empty
(define (empty? heap)
  (empty-node? (heap-root heap)))

; insert a value into a splay heap
(define (insert heap pivot)
  (define < (heap-< heap))
  
  ; split a node into a tree of < nodes and non < nodes
  (define (partition node)
    (cond
      ; no values
      [(empty-node? node)
       (values empty-node empty-node)]
      ; new node will go left
      [(< pivot (node-value node))
       (cond
         ; and there's nothing the other way
         [(empty-node? (node-left node))
          (values empty-node node)]
         ; right than right
         [(< pivot (node-value (node-left node)))
          (define-values (left right)
            (partition (node-left (node-left node))))
          (values left
                  (make-node (node-value (node-left node))
                             right
                             (make-node (node-value node)
                                        (node-right (node-left node))
                                        (node-right node))))]
         ; right then left
         [else
          (define-values (left right)
            (partition (node-right (node-left node))))
          (values (make-node (node-value (node-left node))
                             (node-left (node-left node))
                             left)
                  (make-node (node-value node)
                             right
                             (node-right node)))])]
      ; new node will go right
      [else
       (cond
         ; and there's nothing there
         [(empty-node? (node-right node))
          (values node empty-node)]
         ; right than right
         [(< pivot (node-value (node-right node)))
          (define-values (left right)
            (partition (node-left (node-right node))))
          (values (make-node (node-value node)
                             (node-left node)
                             left)
                  (make-node (node-value (node-right node))
                             right
                             (node-right (node-right node))))]
         ; right than left
         ; this is the rotation case
         [else
          (define-values (left right)
            (partition (node-right (node-right node))))
          (values (make-node (node-value (node-right node))
                             (make-node (node-value node)
                                        (node-left node)
                                        (node-left (node-right node)))
                             left)
                  right)])]))
  
  ; insert the node
  (define-values (left right)
    (partition (heap-root heap)))
  
  (make-heap (make-node pivot left right) <))

; get the smallest node from the heap
(define (first heap)
  (let loop ([node (heap-root heap)])
    (cond
      [(empty-node? node)
       (error 'first "empty heap")]
      [(empty-node? (node-left node))
       (node-value node)]
      [else
       (loop (node-left node))])))

; get the heap without the smallest node
(define (rest heap)
  (make-heap
   (let loop ([node (heap-root heap)])
     (cond
       [(empty-node? node)
        (error 'rest "empty heap")]
       [(empty-node? (node-left node))
        (node-right node)]
       [(empty-node? (node-left (node-left node)))
        (make-node (node-value node)
                   (node-right (node-left node))
                   (node-right node))]
       ; left than left
       ; this is the rotation case
       [else
        (make-node (node-value (node-left node))
                   (loop (node-left (node-left node)))
                   (make-node (node-value node)
                              (node-right (node-left node))
                              (node-right node)))]))
   (heap-< heap)))

; sort using a heap
(define (heapsort ls <)
  (let loop ([ls ls] [heap (make-heap empty-node <)])
    (if (null? ls)
        (let loop ([heap heap])
          (if (empty? heap)
              '()
              (cons (first heap) (loop (rest heap)))))
        (loop (cdr ls) (insert heap (car ls))))))

; randomized testing
(require rackunit)
(for ([i (in-range 100)])
  (define ls (for/list ([i 20]) (random 100)))
  (check-equal? (heapsort ls <) (sort ls <)))