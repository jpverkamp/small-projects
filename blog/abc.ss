; keep only unique items in a list
(define (unique l) (foldl (lambda (x l) (if (member x l) l (cons x l))) '() l))

; test if i evenly divides n
(define (div? n i) (zero? (remainder n i)))

; test if any item in l satisifies the predicate ?
(define (any? ? l) (not (null? (filter ? l))))

; return the prime factors of n
(define (factor n)
  (let loop ([i 2])
    (cond
      [(> (* i i) n) (list n)]
      [(div? n i)
       (append (factor i) (factor (/ n i)))]
      [else 
       (loop (+ i 1))])))

; calculate the greatest common demoninator of a and b
(define (gcd a b)
  (cond
    [(= a b) a]
    [(< a b) (gcd a (- b a))]
    [else (gcd (- a b) b)]))

; test if two numbers are co-prime (share no common factors except 1)
(define (coprime? a b) (= 1 (gcd a b)))

; calculate the radical (product of unique prime factors) of a number
(define (rad n) (apply * (unique (factor n))))

; calculate the ration of radicals for a set of coprime a + b = c
(define (q a b c) (/ (log c) (log (rad (* a b c)))))

; run the test
; for all b, for all a < b; 
;   if a+b < limit, a and b are coprime, and q > 1, store the result 
(define (abc n)
  (sort 
   (for*/list ([b (in-range 1 n)]
               [a (in-range 1 b)]
               #:when (and (< (+ a b) n)
                           (coprime? a b)
                           (> (q a b (+ a b)) 1)))
     (list (q a b (+ a b)) a b (+ a b)))
   (lambda (x y) (> (car x) (car y)))))