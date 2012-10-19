(load "pmatch.ss")

; lookup an item in a balanced binary tree
(define (tree-lookup size tr i)
  (pmatch (list size tr i)
    [(,size (Leaf ,x) 0)
     x]
    [(,size (Leaf ,x) ,i)
     (error 'tree-lookup "subscript-error")]
    [(,size (Node ,x ,t1 ,t2) 0)
     x]
    [(,size (Node ,x ,t1 ,t2) ,i)
     (let ([size^ (div size 2)])
       (if (<= i size^)
           (tree-lookup size^ t1 (- i 1))
           (tree-lookup size^ t2 (- i 1 size^))))]))

; update a balanced binary tree with a new element
(define (tree-update size tr i y)
  (pmatch (list size tr i y)
    [(,size (Leaf ,x) 0 ,y)
     `(Leaf ,y)]
    [(,size (Leaf ,x) ,i ,y)
     (error 'tree-update "subscript error")]
    [(,size (Node ,x ,t1 ,t2) 0 ,y)
     `(Node ,y ,t1 ,t2)]
    [(,size (Node ,x ,t1 ,t2) ,i ,y)
     (let ([size^ (div size 2)])
       (if (<= i size^)
           `(Node ,x ,(tree-update size^ t1 (- i 1) y) ,t2)
           `(Node ,x ,t1 ,(tree-update size^ t2 (- i 1 size^) y))))]))

; test for an empty random access list
; isempty in the paper
(define (*null? ls)
  (null? ls))

; build a random access list
(define (*cons x xs)
  (pmatch xs
    [((,size1 ,t1) (,size2 ,t2) . ,rest)
     (if (= size1 size2)
         `((,(+ 1 size1 size2) (Node ,x ,t1 ,t2)) . ,rest)
         `((1 (Leaf ,x)) . ,xs))]
    [else
     `((1 (Leaf ,x)) . ,xs)]))

; return the first item in a random access list
; head in the paper
(define (*car ls)
  (pmatch ls
    [() (error '*car "empty list")]
    [((,size (Leaf ,x)) . ,rest)
     x]
    [((,size (Node ,x ,t1 ,t2)) . ,rest)
     x]))

; return all but the first item of a random access list
; tail in the paper
(define (*cdr ls)
  (pmatch ls
    [() (error '*cdr "empty list")]
    [((,size (Leaf ,x)) . ,rest)
     rest]
    [((,size (Node ,x ,t1 ,t2)) . ,rest)
     (let ([size^ (div size 2)])
       `((,size^ ,t1) (size^ ,t2) . ,rest))]))

; pull an item out of the list in O(log(n)) time
; lookup in the paper
(define (*list-ref ls i)
  (pmatch (list ls i)
    [(() ,i)
     (error '*list-ref "subscript error")]
    [(((,size ,t) . ,rest) ,i)
     (if (< i size)
         (tree-lookup size t i)
         (*list-ref rest (- i size)))]))

; return a new random access list with the element at the specified index changed
; update in the paper
(define (*list-set ls i y)
  (pmatch (list ls i y)
    [(((,size ,t) . ,rest) ,i ,y)
     (if (< i size)
         `((,size ,(tree-update size t i y)) . ,rest)
         `((,size ,t) . ,(*list-set rest (- i size) y)))]))
