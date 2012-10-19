#lang racket

#|
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

    (3)The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
|#

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
  
