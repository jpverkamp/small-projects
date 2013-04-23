#lang racket

(require rackunit)

(provide
 (contract-out 
  (list->cycle (-> list? cycle?))
  (cycle->list (-> cycle? list?))
  (cycle-head (-> cycle? any))
  (cycle-tail (-> cycle? cycle?))
  (cycle-length (-> cycle? boolean?))
  (cycle-take (-> (and/c integer? positive?) cycle? list?))
  (cycle-reset? (-> cycle? boolean?))
  (cycle-equal? (-> cycle? cycle? boolean?))
  (list-cycle-equal? (-> list? list? boolean?))))

; ----- Cycle ADT -----

; Store a cycle as the current head and original (reset) head
; Length is cached to make cycle-length amortized O(1)
(define-struct cycle (len current original) #:mutable)

; Convert a list to a cycle
(define (list->cycle ls)
  (make-cycle #f ls ls))

; Convert a cycle to a list
(define (cycle->list c)
  (cycle-take (cycle-length c) c))

; Return the first item of a cycle
(define (cycle-head c)
  (if (null? (cycle-current c))
      (car (cycle-original c))
      (car (cycle-current c))))

(check-equal? (cycle-head (list->cycle '(1))) 1)
(check-equal? (cycle-head (list->cycle '(1 2 3))) 1)

; Return all but the first item of a cycle
(define (cycle-tail c)
  (make-cycle 
   (cycle-len c) 
   (cdr ((if (null? (cycle-current c)) cycle-original cycle-current) c))
   (cycle-original c)))

(check-equal? (cycle-head (cycle-tail (list->cycle '(1)))) 1)
(check-equal? (cycle-head (cycle-tail (list->cycle '(1 2 3)))) 2)

; Get the length of a cycle
(define (cycle-length c)
  (unless (cycle-len c)
    (set-cycle-len! c (length (cycle-original c))))
  (cycle-len c))

(check-equal? (cycle-length (list->cycle '(1 2 3))) 3)

; Take the first n items from a cycle
(define (cycle-take n c)
  (let loop ([i 0] [c c])
    (if (= i n)
        '()
        (cons (cycle-head c) (loop (+ i 1) (cycle-tail c))))))

(check-equal? (cycle-take 1 (list->cycle '(1 2 3))) '(1))
(check-equal? (cycle-take 3 (list->cycle '(1 2 3))) '(1 2 3))
(check-equal? (cycle-take 5 (list->cycle '(1 2 3))) '(1 2 3 1 2))
(check-equal? (cycle-take 7 (list->cycle '(1 2 3))) '(1 2 3 1 2 3 1))

; Test if a cycle is about to reset
(define (cycle-reset? c)
  (null? (cycle-current c)))

(check-true (cycle-reset? (cycle-tail (cycle-tail (cycle-tail (list->cycle '(1 2 3)))))))
(check-false (cycle-reset? (list->cycle '(1 2 3))))
(check-false (cycle-reset? (cycle-tail
                            (cycle-tail
                             (cycle-tail
                              (cycle-tail (list->cycle '(1 2 3))))))))

; ----- Compare cycles by checking all pairwise first positions -----

; Test if two cycles are equal
(define (cycle-equal? c1 c2)
  ; Check the lengths first
  (define len (cycle-length c1))
  (and (= len (cycle-length c2))
       (let loop ([ci1 c1] [ci2 c2])
         (cond
           ; No matches found
           [(cycle-reset? ci1)
            #f]
           ; No match found for this start in c1
           ; Advance c1, reset c2
           [(cycle-reset? ci2)
            (loop (cycle-tail ci1) c2)]
           ; Match found at the current element!
           [(equal? (cycle-take len ci1)
                    (cycle-take len ci2))
            #t]
           ; Otherwise, no match, advance c2
           [else
            (loop ci1 (cycle-tail ci2))]))))

(check-true (cycle-equal? (list->cycle '(1 2 3 4 5)) (list->cycle '(1 2 3 4 5))))
(check-true (cycle-equal? (list->cycle '(1 2 3 4 5)) (list->cycle '(3 4 5 1 2))))
(check-true (cycle-equal? (list->cycle '(1 2 2 1)) (list->cycle '(2 1 1 2))))

(check-false (cycle-equal? (list->cycle '(1 1)) (list->cycle '(1 1 1 1))))
(check-false (cycle-equal? (list->cycle '(1 2 3 4)) (list->cycle '(1 2 3 5))))

; ----- Compare cycles by doubling one list and checking the other as a subset -----

; Check if p is a prefix of ls
(define (prefix? ls p)
  (or (null? p)
      (and (equal? (car ls) (car p))
           (prefix? (cdr ls) (cdr p)))))

; Check if a list needle is in the list haystack
(define (contains? haystack needle)
  (and (not (null? haystack))
       (or (prefix? haystack needle)
           (contains? (cdr haystack) needle))))

; Check if two cycles (as lists) are equal by doubling one
(define (list-cycle-equal? lsc1 lsc2)
  (and (= (length lsc1) (length lsc2))
       (contains? (append lsc1 lsc1) lsc2)))

(check-true (list-cycle-equal? '(1 2 3 4 5) '(1 2 3 4 5)))
(check-true (list-cycle-equal? '(1 2 3 4 5) '(3 4 5 1 2)))
(check-true (list-cycle-equal? '(1 2 2 1) '(2 1 1 2)))

(check-false (list-cycle-equal? '(1 1) '(1 1 1 1)))
(check-false (list-cycle-equal? '(1 2 3 4) '(1 2 3 5)))

; ----- Compare cycles by lexically sorting cycles -----

; Advance a cycle to the lexically minimum position
(define (cycle-lexical-min c [< <] [= =])
  ; Check if one cycle is less than another
  (define (cycle-< c1 c2)
    (let loop ([c1 c1] [c1-cnt (cycle-length c1)]
               [c2 c2] [c2-cnt (cycle-length c2)])
      (and (> c1-cnt 0)
           (> c2-cnt 0)
           (or (< (cycle-head c1) (cycle-head c2))
               (and (= (cycle-head c1) (cycle-head c2))
                    (loop (cycle-tail c1) (- c1-cnt 1)
                          (cycle-tail c2) (- c2-cnt 1)))))))
  
  ; Lexically sort by storing minimum
  (let loop ([min c] [c (cycle-tail c)])
    (cond
      [(cycle-reset? c) min]
      [(cycle-< c min) (loop c (cycle-tail c))]
      [else (loop min (cycle-tail c))])))

(check-equal? (cycle->list (cycle-lexical-min (list->cycle '(1 2 3 4)))) '(1 2 3 4))
(check-equal? (cycle->list (cycle-lexical-min (list->cycle '(3 4 1 2)))) '(1 2 3 4))
(check-equal? (cycle->list (cycle-lexical-min (list->cycle '(1 2 2 1)))) '(1 1 2 2))
(check-equal? (cycle->list (cycle-lexical-min (list->cycle '(2 1 1 2)))) '(1 1 2 2))
(check-equal? (cycle->list (cycle-lexical-min (list->cycle '(1 1 1)))) '(1 1 1))

; Compare cycles by lexical comparison
(define (lexical-cycle-equal? c1 c2 [< <] [= =])
  (equal? (cycle-take (cycle-length c1) (cycle-lexical-min c1 < =))
          (cycle-take (cycle-length c2) (cycle-lexical-min c2 < =))))

(check-true (lexical-cycle-equal? (list->cycle '(1 2 3 4 5)) (list->cycle '(1 2 3 4 5)) < =))
(check-true (lexical-cycle-equal? (list->cycle '(1 2 3 4 5)) (list->cycle '(3 4 5 1 2)) < =))
(check-true (lexical-cycle-equal? (list->cycle '(1 2 2 1)) (list->cycle '(2 1 1 2)) < =))

(check-false (cycle-equal? (list->cycle '(1 1)) (list->cycle '(1 1 1 1))))
(check-false (lexical-cycle-equal? (list->cycle '(1 2)) (list->cycle '(1 2 3 4)) < =))
(check-false (lexical-cycle-equal? (list->cycle '(1 2 3 4)) (list->cycle '(1 2 3 5)) < =))