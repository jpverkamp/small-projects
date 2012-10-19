; convert a number into a list of digits
(define (digits n)
  (if (zero? n)
      '()
      (cons (mod n 10) (digits (div n 10)))))

; square a number
(define (sqr n)
  (* n n))

; return either the terminal number (should be 1)
; or the point at which a cycle is detected
(define (happy n)
  (let ^ ([n n] [ns '()])
    (if (member n ns)
        n
        (^ (apply + (map sqr (digits n))) (cons n ns)))))

; go through all of the numbers, check if we have the cycle or 1
; print and exit if we find a counter example
(define (test)
  (let ([cycle '(1 4 16 37 58 89 145 42 20)])
    (let ^ ([i 1])
      (let ([res (happy i)])
        (when (not (member res cycle))
              (printf "~s\t~s\n" i res)
              (exit))
        (^ (add1 i))))))