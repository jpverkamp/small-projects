; given any random die and a possible outcome
; turn it into an unfair coin
(define (die->coin possible die)
  (lambda ()
    (eq? possible (die))))

; given any coin, return a fair 50/50 coin
(define (coin->fair-coin coin)
  (lambda ()
    (let loop ([a (coin)]
               [b (coin)])
      (if (or (and a (not b))
              (and b (not a)))
          a
          (loop (coin) (coin))))))

; given a fair 50/50 coin, randomly choose an item uniformly from a list
(define (fair-coin->uniform coin ls)
  (let loop ([l ls])
    (cond
      [(null? l) (fair-coin->uniform coin ls)]
      [(null? (cdr l)) (car l)]
      [else (loop (filter (lambda (_) (coin)) l))])))

; now, convert one random die to another
; assume that rand-in can generate a 1 and that max-out is n for [1..n]
(define (rand->rand rand-in max-out)
  (fair-coin->uniform
    (coin->fair-coin
      (die->coin 1 rand-in))
    (map add1 (iota max-out))))

; now, actually do the problem
(define (rand3) (add1 (random 3)))
(define (rand5) (add1 (random 5)))
(define (rand9) (rand->rand rand3 9))
(define (rand7) (rand->rand rand5 7))

; remove duplicates
(define (unique l)
  (fold-right (lambda (x l) (if (member x l) l (cons x l))) '() l))

; helper to test if a die is uniform with n rounds of testing
(define (test-uniform die n-rounds)
  (let ([ls (map (lambda (_) (die)) (iota n-rounds))])
    (for-each
      (lambda (i)
        (printf "~a = ~a%\n"
          i
          (* 100.0 (/ (length (filter (lambda (n) (= n i)) ls)) n-rounds))))
      (sort < (unique ls)))))