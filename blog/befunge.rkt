#lang racket

(require (except-in 2htdp/universe state)
         (only-in 2htdp/image place-image empty-scene)
         racket/draw
         pict)

(define (mod a b)
  (let ([r (remainder a b)])
    (if (< r 0) (+ r b) r)))

; Vector methods that wrap out of bounds indices
(define (wrapped-vector-ref v i) (vector-ref v (mod i (vector-length v))))
(define (wrapped-vector-set! v i x) (vector-set! v (mod i (vector-length v)) x))

; Helper methods 
(define (grid-ref grid x y) (wrapped-vector-ref (wrapped-vector-ref grid y) x))
(define (grid-set! grid x y v) (wrapped-vector-set! (wrapped-vector-ref grid y) x v))

; Read a befuge program into a two dimensional array of characters
; Befunge programs are ended either by an end of file or a blank line
(define (read-befunge [in (current-input-port)])
  ; Read the basic data
  (define grid
    (for/vector ([y (in-naturals)] [line (in-lines in)] #:break (equal? "" (string-trim line)))
      (for/vector ([x (in-naturals)] [c (in-string line)])
        c)))
  
  (define max-x (apply max (for/list ([row (in-vector grid)]) (vector-length row))))
  
  ; Add whitespace to the right ends if necessary
  (for/vector ([y (in-naturals)] [row (in-vector grid)])
    (for/vector ([x (in-range max-x)])
      (if (< x (vector-length row))
          (vector-ref row x)
          #\space))))

; Write out a befunge program in a from matching read-befunge
(define (write-befunge grid [out (current-output-port)])
  (for ([row (in-vector grid)])
    (for ([c (in-vector row)])
      (display c out))
    (newline out)))

; Render the current state of a befunge program to a bitmap
(define (render-befunge grid [active-x #f] [active-y #f] [stack '()] [output ""])
  (define default-color (send the-color-database find-color "black"))
  (define active-color (send the-color-database find-color "red"))
  
  (when active-x (set! active-x (mod active-x (vector-length (vector-ref grid 0)))))
  (when active-y (set! active-y (mod active-y (vector-length grid))))
  
  (define picts
    (for/list ([y (in-naturals)] [row (in-vector grid)])
      (for/list ([x (in-naturals)] [char (in-vector row)])
        (define active 
          (and active-x (= x active-x)
               active-y (= y active-y)))
        
        (cc-superimpose
         (colorize (filled-rectangle 10 10) (if active "red" "white"))
         (text (string char) 
               'modern)))))

  (define grid-pict
    (apply vc-append (map (λ (row) (apply hc-append row)) picts)))
  
  (vl-append 5.0 
             grid-pict
             (text (format "stack: ~a" stack))
             (text (format "output: ~a" output))))
  
; Befunge state
(struct state (x y facing stack grid running) #:transparent #:mutable)
(define current-state (make-parameter #f))

 ; Stack maniuplation. Pop 0 on an empty stack
(define (pop!)
  (when (current-state)
    (define stack (state-stack (current-state)))
    (if (null? stack)
        0
        (begin0
          (car stack)
          (set-state-stack! (current-state) (cdr stack))))))

(define (push! v)
  (when (current-state)
    (set-state-stack! (current-state) (cons v (state-stack (current-state))))))

; Move in the current direction
(define (move!)
  (when (current-state)
    (case (state-facing (current-state))
      [(right) (set-state-x! (current-state) (+ (state-x (current-state)) 1))]
      [(left)  (set-state-x! (current-state) (- (state-x (current-state)) 1))]
      [(down)  (set-state-y! (current-state) (+ (state-y (current-state)) 1))]
      [(up)    (set-state-y! (current-state) (- (state-y (current-state)) 1))])))

; Get the current character from the state
(define (grid-@)
  (if (current-state)
      (grid-ref (state-grid (current-state)) 
                (state-x (current-state)) 
                (state-y (current-state)))
      #\space))

; Advance the befunge program to the next step
(define (step!)
  (when (and (current-state) (state-running (current-state)))
    ; Decode the next instruction
    (define cmd (grid-@))
    (case cmd
      ; Push this number on the stack
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (push! (string->number (string cmd)))]
      ; Addition: Pop a and b, then push a+b
      ; Subtraction: Pop a and b, then push b-a
      ; Multiplication: Pop a and b, then push a*b
      ; Integer division: Pop a and b, then push b/a, rounded down. If a is zero, ask the user what result they want.[dubious – discuss]
      ; Modulo: Pop a and b, then push the remainder of the integer division of b/a. If a is zero, ask the user what result they want.[dubious – discuss]
      ; Greater than: Pop a and b, then push 1 if b>a, otherwise zero.
      [(#\+ #\- #\* #\/ #\% #\`)
       (define a (pop!))
       (define b (pop!))
       (define op (cdr (assoc cmd `((#\+ . ,+) 
                                    (#\- . ,-) 
                                    (#\* . ,*)
                                    (#\/ . ,quotient) 
                                    (#\% . ,remainder)
                                    (#\` . ,(λ (b a) (if (> b a) 1 0)))))))
       (push! (op b a))]
      ; Logical NOT: Pop a value. If the value is zero, push 1; otherwise, push zero.
      [(#\!)
       (push! (if (zero? (pop!)) 1 0))]
      ; Change direction
      [(#\> #\< #\^ #\v)
       (set-state-facing! (current-state) (cdr (assoc cmd `((#\> . right) 
                                                            (#\< . left) 
                                                            (#\^ . up) 
                                                            (#\v . down)))))]
      ; Start moving in a random cardinal direction
      [(#\?)
       (set-state-facing! (current-state) (list-ref `(right left up down) (random 4)))]
      ; Pop a value; move right if value=0, left otherwise
      [(#\_)
       (set-state-facing! (current-state) (if (zero? (pop!)) 'right 'left))]
      ; Pop a value; move down if value=0, up otherwise
      [(#\|)
       (set-state-facing! (current-state) (if (zero? (pop!)) 'down 'up))]
      ; Start string mode: push each character's ASCII value all the way up to the next "
      ; Note: uses one fuel
      [(#\")
       (let loop ()
         (move!)
         (define c (grid-@))
         (when (not (equal? c #\"))
           (push! (char->integer c))
           (loop)))]
      ; Duplicate value on top of the stack
      [(#\:)
       (define v (pop!))
       (push! v)
       (push! v)]
      ; Swap two values on top of the stack
      [(#\\)
       (define a (pop!))
       (define b (pop!))
       (push! a)
       (push! b)]
      ; Pop value from the stack and discard it
      [(#\$)
       (pop!)]
      ; Pop value and output as an integer
      [(#\.)
       (display (pop!))
       (display " ")]
      ; Pop value and output as ASCII character
      [(#\,)
       (display (integer->char (pop!)))]
      ; Trampoline: Skip next cell
      [(#\#)
       (move!)]
      ; A "put" call (a way to store a value for later use).
      ; Pop y, x and v, then change the character at the position (x,y)
      ; in the program to the character with ASCII value v
      [(#\p)
       (define y (pop!))
       (define x (pop!))
       (define v (pop!))
       (grid-set! (state-grid (current-state))
                  x y 
                  (with-handlers ([exn? (λ _ v)])
                    (integer->char v)))]
      ; A "get" call (a way to retrieve data in storage). 
      ; Pop y and x, then push ASCII value of the character at that position in the program
      [(#\g)
       (define y (pop!))
       (define x (pop!))
       (define v (grid-@))
       (push! (if (number? v) v (char->integer v)))]
      ; Ask user for a number and push it
      [(#\&)
       (display "Enter a number: ")
       (push! (read))]
      ; Ask user for a character and push its ASCII value
      [(#\~)
       (display "Enter a character: ")
       (push! (read-char))
       (newline)]
      ; End program
      [(#\@)
       (set-state-running! (current-state) #f)])
    
    (move!)))

; Run a befunge program
(define (run-befunge [input      (current-input-port)] 
                     #:debug     [debug #f] 
                     #:fuel      [fuel +inf.0]
                     #:visualize [visualize #f])
  ; Load the grid
  (define grid
    (cond
      [(input-port? input) (read-befunge input)]
      [(string? input) (call-with-input-string input read-befunge)]
      [(vector? input) input]))
  
  ; Set up initial state (x y facing stack grid running)
  (parameterize ([current-state (state 0 0 'right '() grid #t)])
    (cond 
      ; Render the output, using big-bang to run the loop
      [visualize
       (define current-output (make-parameter ""))
       
       (define (tick i)
         (when (and (state-running (current-state))
                    (< i fuel))
           (define new-output (with-output-to-string step!))
           (current-output (string-append (current-output) new-output)))
         (+ i 1))
         
       (define (draw i)
         (define pic
           (render-befunge (state-grid (current-state))
                           (state-x (current-state))
                           (state-y (current-state))
                           (state-stack (current-state))
                           (current-output)))
         
         (place-image (pict->bitmap pic)
                      (+ 5 (/ (pict-width pic) 2))
                      (+ 5 (/ (pict-height pic) 2))
                      (empty-scene (+ 10 (* 2 (pict-width pic)))
                                   (+ 10 (pict-height pic)))))
       
       (define (stop? i)
         (or (not (state-running (current-state)))
             (>= i fuel)))
       
       (big-bang 0
         [on-tick tick 0.10]
         [to-draw draw]
         [stop-when stop?]
         [record? #t])]
      
      ; Don't render, just run the program
      [else
       (let loop ([step 0])
         (when (and (state-running (current-state))
                    (< step fuel))
           (when debug
             (write-befunge (state-grid (current-state)))
             (printf "~a x ~a, ~a\nstack: ~a\n\n" 
                     (state-x (current-state))
                     (state-y (current-state))
                     (state-facing (current-state))
                     (state-stack (current-state))))
           
           (step!)
           (loop (+ step 1))))])))
