#lang racket

(require racket/generator)

(define (make-pi-spigot)
  (generator ()
    (let loop ([q 1] [r 0] [t 1] [k 1] [n 3] [l 3])
      (if (< (- (+ (* 4 q) r) t) (* n t))
          (begin
            (yield n)
            (loop (* 10 q)
                  (* 10 (- r (* n t)))
                  t
                  k
                  (- (quotient (* 10 (+ (* 3 q) r)) t) (* 10 n))
                  l))
          (loop (* q k)
                (* (+ (* 2 q) r) l)
                (* t l)
                (+ k 1)
                (quotient (+ (* q (+ (* 7 k) 2)) (* r l)) (* t l))
                (+ l 2))))))

(define (any? ? ls)
  (not (null? (filter ? ls))))

(define (all? ? ls)
  (= (length ls) (length (filter ? ls))))

(define (find-in-pi ls)
  (let ([pi (make-pi-spigot)])
    (pi) ; skip the 3
    (let loop ([i 1] [l* '()])
      (let* ([d (pi)]
             [l* (map cdr (filter (lambda (ea) 
                                    (and (not (null? ea)) 
                                         (= d (car ea))))
                                  (cons ls l*)))])
        (if (any? null? l*) (- i (length ls)) (loop (+ i 1) l*))))))

(define (number-string->digits nstr)
  (map string->number (map string (string->list nstr))))

(define (number->digits n)
  (if (zero? n)
      '(0)
      (let loop ([n n] [a '()])
        (if (zero? n)
            a
            (loop (quotient n 10) (cons (remainder n 10) a))))))

(for ([n (current-command-line-arguments)])
  (printf "~a is ~a digits into pi\n" n (find-in-pi (number-string->digits n))))
   
(define (digits-of-pi n)
  (for/list ([i (in-range (+ n 1))]
             [d (in-producer (make-pi-spigot) #f)])
    d))

(define (pi->string n)
  (string-append 
   "3."                 
   (apply
    string-append
    (map number->string (cdr (digits-of-pi n))))))
  
