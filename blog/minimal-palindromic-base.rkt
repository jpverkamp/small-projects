#lang racket

(require plot)

; Convert a decimal number n to base b
(define (rebase n b)
  (let loop ([n n] [ls '()])
     (if (= n 0)
        ls
        (loop (quotient n b) 
              (cons (remainder n b) ls)))))

; Find the minimal base b such that n in base b is a palindrome
(define (minimal-palindromic-base n)
  (for/first ([b (in-naturals 2)]
              #:when (let ([nb (rebase n b)])
                       (equal? nb (reverse nb))))
    b))

; Find the number n which has the largest palindromic base
(define (maximal-minimal-palindromic-base n-min n-max)
  (for/fold ([b -1] [n #f]) ([i (in-range n-min (+ n-max 1))])
    (define mpb (minimal-palindromic-base i))
    (if (> i b)
        (values mpb i)
        (values b   n))))

; Plot a whole range of minimal palindromic bases
(define (plot-minimal-palindromic-bases n-min n-max)
  (plot (lines (for/list ([i (in-range n-min (+ n-max 1))])
                 (vector i (minimal-palindromic-base i)))
               #:color 6
               #:label "minimal palindromic base")))
