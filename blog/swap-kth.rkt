#lang racket

; Given a list, swap the kth from head and tail
(define (swap-kth ls k)
  ; Get pointers to the kth head and the kth tail
  (define-values (_ kth-head kth-tail)
    (let loop ([i 1] [ls ls])
      (cond
        [(null? ls)
         (values 1 #f #f)]
        [else
         (define-values (j kth-head kth-tail)
           (loop (+ i 1) (cdr ls)))
         (values
          (+ j 1)
          (if (= i k) ls kth-head)
          (if (= j k) ls kth-tail))])))
  
  ; Recur again, swapping the pointers
  (let loop ([ls ls])
    (cond
      [(null? ls)        '()]
      [(eq? ls kth-head) (cons (car kth-tail) (loop (cdr ls)))]
      [(eq? ls kth-tail) (cons (car kth-head) (loop (cdr ls)))]
      [else              (cons (car ls) (loop (cdr ls)))])))

; Make sure that everything works as it should
(module+ test
  (require rackunit)
  (check-equal? (swap-kth '() 3) '())
  (check-equal? (swap-kth '(1) 3) '(1))
  (check-equal? (swap-kth '(1 2) 3) '(1 2))
  (check-equal? (swap-kth '(1 2 3) 3) '(3 2 1))
  (check-equal? (swap-kth '(1 2 3 4) 3) '(1 3 2 4))
  (check-equal? (swap-kth '(1 2 3 4 5) 3) '(1 2 3 4 5))
  (check-equal? (swap-kth '(1 2 3 4 5 6) 3) '(1 2 4 3 5 6))
  (check-equal? (swap-kth '(1 2 3 4 5 6 7) 3) '(1 2 5 4 3 6 7))
  (check-equal? (swap-kth '(1 2 3 4 5 6 7 8) 3) '(1 2 6 4 5 3 7 8))
  (check-equal? (swap-kth '(1 2 3 4 5 6 7 8 9) 3) '(1 2 7 4 5 6 3 8 9)))