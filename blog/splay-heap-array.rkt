#lang racket

(provide (rename-out [new-heap make-heap])
         empty?
         push!
         pop!
         heapsort)

; wrap a heap in a struct
(define-struct heap (data size <) 
  #:transparent
  #:mutable)

; create a new heap with a given comparator
(define (new-heap <)
  (make-heap (make-vector 1 #f) 0 <))

; test if a heap is empty
(define (empty? heap)
  (zero? (heap-size heap)))

; grow a heap
; if it's small, double in size; otherwise 1.5x
(define (grow! heap)
  (define old-size (heap-size heap))
  (define new-size (* old-size (if (< old-size 64) 2 3/2)))
  (set-heap-data!
   heap
   (for/vector ([i (in-range new-size)])
     (and (< i old-size)
          (vector-ref (heap-data heap) i)))))
                                 
; insert an item into a heap
(define (push! heap value)
  (when (= (heap-size heap) (vector-length (heap-data heap)))
    (grow! heap))
  
  (define size (heap-size heap))
  (set-heap-size! heap (+ 1 size))
  
  (if (= 1 (heap-size heap))
      (vector-set! (heap-data heap) 0 value)
      (sift-up! heap size value)))

; return the mimum element in a heap
(define (peek heap)
  (when (= 0 (heap-size heap))
    (error 'peek "empty heap"))
  
  (vector-ref (heap-data heap) 0))

; pop! the minimum element in a heap (and return it)
(define (pop! heap)
  (when (= 0 (heap-size heap))
    (error 'pop! "empty heap"))
  
  (define result (peek heap))
  (define last (vector-ref (heap-data heap) (+ -1 (heap-size heap))))
  
  (set-heap-size! heap (+ -1 (heap-size heap)))
  (unless (zero? (heap-size heap))
    (sift-down! heap 0 last))
  (vector-set! (heap-data heap) (heap-size heap) #f)
  
  result)

; re-sift the heap upwards
(define (sift-up! heap index value)
  (define parent (arithmetic-shift (+ -1 index) -1))
  (cond
    ; at the top, we have to stop here
    ; or the parent is already smaller
    [(or (= index 0)
         (not ((heap-< heap) value (vector-ref (heap-data heap) parent))))
     (vector-set! (heap-data heap) index value)]
    ; otherwise sift, move the parent down and recur
    [else
     (vector-set! (heap-data heap) 
                  index 
                  (vector-ref (heap-data heap) parent))
     (sift-up! heap parent value)]))

; re-sift the heap downwards
(define (sift-down! heap index value)
  
  (define left (+ 1 (arithmetic-shift index 1)))
  (define right (+ 1 left))
  
  (cond
    ; we're out in the branches or smaller than children
    [(and (or (>= left (heap-size heap))
              ((heap-< heap) value (vector-ref (heap-data heap) left)))
          (or (>= right (heap-size heap))
              ((heap-< heap) value (vector-ref (heap-data heap) right))))
     (vector-set! (heap-data heap) index value)]
    ; only a left, but it's smaller
    ; or left is smaller than right, recur left
    [(or (and (>= right (heap-size heap))
              ((heap-< heap) value (vector-ref (heap-data heap) left)))
         ((heap-< heap) (vector-ref (heap-data heap) left)
                        (vector-ref (heap-data heap) right)))
     (vector-set! (heap-data heap)
                  index
                  (vector-ref (heap-data heap) left))
     (sift-down! heap left value)]
    ; otherwise, recur to the right
    [else
     (vector-set! (heap-data heap)
                  index
                  (vector-ref (heap-data heap) right))
     (sift-down! heap right value)]))
     
; sort a list using a heap
(define (heapsort ls <)
  (define heap (new-heap <))
  
  (let loop ([ls ls])
    (unless (null? ls)
      (push! heap (car ls))
      (loop (cdr ls))))

  (let loop ()
    (if (empty? heap)
        '()
        (cons (pop! heap) (loop)))))

; validate that a heap has a valid structure
(define (valid-heap? heap)
  (let loop ([i 0])
    (or (>= i (heap-size heap))
        (and (or (>= (+ (* 2 i) 1) (heap-size heap))
                 (and (not ((heap-< heap) (vector-ref (heap-data heap) (+ (* 2 i) 1))
                                          (vector-ref (heap-data heap) i)))
                      (loop (+ (* 2 i) 1))))
             (or (>= (+ (* 2 i) 2) (heap-size heap))
                 (and (not ((heap-< heap) (vector-ref (heap-data heap) (+ (* 2 i) 2))
                                          (vector-ref (heap-data heap) i)))
                      (loop (+ (* 2 i) 2))))))))

; randomized testing
(require rackunit)
(for ([i (in-range 100)])
  (define ls (for/list ([i 20]) (random 100)))
  (check-equal? (heapsort ls <) (sort ls <)))
