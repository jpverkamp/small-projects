(import (c211 turtle))
(import (c211 image))

; draw positions for letters
(define letters
  '#(((0 0) (9 0) (12 3) (9 6) (6 6) (6 0) (6 6) (0 6))   ; A
     ((0 0) (12 0) (12 3) (9 6) (6 3) (6 0) (6 3) (3 6) (0 3) (0 0)) ; B
     ((9 6) (12 3) (9 0) (3 0) (0 3) (3 6))               ; C
     ((0 0) (12 0) (12 3) (9 6) (3 6) (0 3) (0 0))        ; D
     ((12 6) (12 0) (6 0) (6 3) (6 0) (0 0) (0 6))        ; E
     ((12 6) (12 0) (6 0) (6 3) (6 0) (0 0))              ; F
     ((6 3) (6 6) (0 6) (0 3) (6 0) (9 0) (12 3) (12 6))  ; G
     ((12 0) (0 0) (6 0) (6 6) (12 6) (0 6))              ; H
     ((0 0) (0 6) (0 3) (12 3) (12 6) (12 0))             ; I
     ((3 0) (0 3) (3 6) (12 6) (12 3))                    ; J
     ((0 0) (12 0) (6 0) (12 6) (6 0) (0 6))              ; K
     ((12 0) (0 0) (0 6))                                 ; L
     ((0 0) (12 0) (9 3) (12 6) (0 6))                    ; M
     ((0 0) (12 0) (0 6) (12 6))                          ; N
     ((0 3) (3 6) (9 6) (12 3) (9 0) (3 0) (0 3))         ; O
     ((0 0) (12 0) (12 3) (9 6) (6 3) (6 0))              ; P
     ((0 6) (0 3) (3 6) (9 6) (12 3) (9 0) (3 0) (0 3))   ; Q
     ((0 0) (12 0) (12 3) (9 6) (6 3) (6 0) (0 6))        ; R
     ((0 0) (0 3) (3 6) (9 0) (12 3) (12 6))              ; S
     ((12 0) (12 6) (12 3) (0 3))                         ; T
     ((12 0) (3 0) (0 3) (3 6) (12 6))                    ; U
     ((12 0) (0 3) (12 6))                                ; V
     ((12 0) (0 0) (3 3) (0 6) (12 6))                    ; W
     ((0 0) (12 6) (6 3) (12 0) (0 6))                    ; X
     ((0 3) (6 3) (12 0) (6 3) (12 6))                    ; Y
     ((12 0) (12 6) (0 0) (0 6))))                        ; Z

; define positions for digits
(define digits
  '#(((0 3) (3 6) (9 6) (12 3) (9 0) (3 0) (0 3))         ; 0
     ((0 3) (12 3) (9 0))                                 ; 1
     ((0 6) (0 0) (6 6) (9 6) (12 3) (9 0))               ; 2
     ((3 0) (0 3) (3 6) (6 3) (9 6) (12 3) (9 0))         ; 3
     ((6 6) (6 0) (12 6) (0 6))                           ; 4
     ((12 6) (12 0) (9 0) (6 6) (3 6) (0 3) (0 0))        ; 5
     ((9 6) (12 3) (9 0) (3 0) (0 3) (3 6) (6 3) (3 0))   ; 6
     ((12 0) (12 6) (0 3))                                ; 7
     ((3 0) (0 3) (3 6) (9 0) (12 3) (9 6) (3 0))         ; 8
     ((3 0) (0 3) (3 6) (9 6) (12 3) (9 0) (6 3) (9 6))   ; 9
     ((6 0) (6 6))))                                      ; -

; helper to calculate how wide a number is (add one for negative numbers)
(define (digits-in n)
  (if (= n 0)
      0
      (+ (if (< n 0) 1 0)
        (inexact->exact (+ 1 (floor (log (abs n) 10)))))))

; using turtle t, draw the digit/letter with index i from table chars
; with the top left at r, c
; reset the turtle to wherever it was after that
(define (draw-thing chars t r c i)
  (block t
    (let ([ps (vector-ref chars i)])
      (lift-pen! t)
      (move-to! t (+ c (cadar ps)) (+ r (caar ps)))
      (drop-pen! t)
      (let loop ([ps (cdr ps)])
        (unless (null? ps)
          (move-to! t (+ c (cadar ps)) (+ r (caar ps)))
          (loop (cdr ps)))))))

; using turtle t, draw a string s with the top left at r, c
; reset the turtle to whever it was after that
(define (draw-string t r c s)
  (block t
    (for-each
      (lambda (i)
        (unless (eq? #\space (string-ref s i))
          (draw-thing letters t r (+ c (* i 10))
            (- (char->integer (char-upcase (string-ref s i))) 65))))
      (iota (string-length s)))))

; using turtle t, draw a number n with the top left at r, c
; reset the turtle to whever it was after that
(define (draw-number t r c n)
  (block t
    (if (= n 0)
        (draw-thing digits t r (+ c 10) 0)
        (let ([? (< n 0)])
          (let loop ([c (+ c (* 10 (digits-in n)) 1)]
                     [n (abs n)])
            (when (> n 0)
              (draw-thing digits t r c (mod n 10))
              (loop (- c 10) (div n 10)))
            (when (and ? (= n 0))
              (draw-thing digits t r c 10)))))))

; draw a temperature table
; (draw-image (make-temperature-table))
(define (make-temperature-table)
  (let ([t (hatch)])
    (draw-string t 15 0 " F")
    (draw-string t 15 50 " C")
    (for-each
      (lambda (row)
        (let* ([f (* row 20)]
               [c (inexact->exact (round (/ (* (- f 32) 5) 9)))])
          (draw-number t (* (- row) 15) 0 f)
          (draw-number t (* (- row) 15) 50 c)))
      (iota 16))
    (turtle->image t)))

; draw hello world
; (draw-image (hello-world))
(define (hello-world)
  (let ([t (hatch)])
    (draw-string t 0 0 "Hello World")
    (turtle->image t)))