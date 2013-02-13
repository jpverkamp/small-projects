#lang racket

(provide tree tree? tree-value tree-left tree-right
         leaf leaf?
         empty empty?
         insert insert-all
         minimum maximum
         predecessor successor)

; set up a tree structure
(define-struct tree (value left right))

(define (leaf val) (tree val (empty-tree) (empty-tree)))
(define (leaf? tr) (and (tree? tr) (not (tree-left? tr)) (not (tree-right? tr))))

(define *empty-tree* (tree (void) (void) (void)))
(define (empty-tree) *empty-tree*)
(define (empty? tr) (eq? tr *empty-tree*))

; test if the left node of a tree is non-empty
(define (tree-left? tr)
  (and (tree? tr)
       (not (empty? (tree-left tr)))))

; test if the right node of a tree is non-empty
(define (tree-right? tr)
  (and (tree? tr)
       (not (empty? (tree-right tr)))))

; find the minimum value in a binary search tree
(define (minimum tr)
  (cond
    [(empty? tr) (void)]
    [(tree-left? tr)
     (minimum (tree-left tr))]
    [else
     (tree-value tr)]))

; find the maximum value in a binary search tree
(define (maximum tr)
  (cond
    [(empty? tr) (void)]
    [(tree-right? tr)
     (maximum (tree-right tr))]
    [else
     (tree-value tr)]))

; insert a value into a binary search tree
(define (insert tr < val)
  (let loop ([tr tr])
    (cond
      [(empty? tr) (leaf val)]
      [(< val (tree-value tr))
       (tree (tree-value tr)
             (loop (tree-left tr))
             (tree-right tr))]
      [else 
       (tree (tree-value tr)
             (tree-left tr)
             (loop (tree-right tr)))])))

; insert a bunch of values all at once
(define (insert-all tr < vals)
  (cond 
    [(null? vals) tr]
    [else 
     (insert-all (insert tr < (car vals)) < (cdr vals))]))

; recur to a value, return: 
;   the last time we went left
;   the node containing the search values
;   the last time we went right
(define (find-nodes tr < val)
  (let loop ([tr tr] [last-left (void)] [last-right (void)])
    (cond
      [(empty? tr) 
       (values last-left tr last-right)]
      [(equal? val (tree-value tr))
       (values last-left tr last-right)]
      [(< val (tree-value tr))
       (loop (tree-left tr) tr last-right)]
      [else
       (loop (tree-right tr) last-left tr)])))

; get the predecessor to a value
(define (predecessor tr < val)
  (define-values (l v r) (find-nodes tr < val))
  (cond
    [(tree-left? v)
     (maximum (tree-left v))]
    [(and (not (void? r)) (not (empty? r)))
     (tree-value r)]))
     
; get the successor to a value
(define (successor tr < val)
  (define-values (l v r) (find-nodes tr < val))
  (cond
    [(tree-right? v)
     (minimum (tree-right v))]
    [(and (not (void? l)) (not (empty? l)))
     (tree-value l)]))

; sort using a tree, using successor
; this is actually O(n log n) believe it or not
(define (tree-sort < ls)
  (define tr (insert-all (empty-tree) < ls))
  (let loop ([x (minimum tr)])
    (cond
      [(void? x) '()]
      [else
       (cons x (loop (successor tr < x)))])))

; sort a tree into reverse order, using predecessor
; this is actually O(n log n) believe it or not
(define (tree-reverse-sort < ls)
  (define tr (insert-all (empty-tree) < ls))
  (let loop ([x (maximum tr)])
    (cond
      [(void? x) '()]
      [else
       (cons x (loop (predecessor tr < x)))])))