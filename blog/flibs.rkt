#lang racket

(provide random-flib string->flib string->input run-flib repeat-string 
         score-flib mutate breed run)

; generate a random flib
; a flib is a state machine of the form
;   0  1
; A 1B 1C
; B 0C 0B
; C 1A 0A => 1B1C0C0B1A0A
(define (random-flib)
  (for/vector ([i 12])
    (if (even? i)
        (random 2)
        (vector-ref '#(A B C) (random 3)))))

; convert a string representation of a flib into the vector we're using
(define (string->flib s)
  (for/vector ([c s])
    (if (char-alphabetic? c)
        (string->symbol (string c))
        (string->number (string c)))))

; convert a string representation of an input into a vector of bits
(define string->input string->flib)

; run a flib on a given input
(define (run-flib flib input)
  ; turn a state into a number
  (define (f s) (case s [(A) 0] [(B) 1] [(C) 2]))
  
  ; initial state
  (define state 'A)

  ; loop over the flip, update state and report output
  (for/vector ([this input])
    (let ([next (vector-ref flib (+ (* (f state) 4) (* this 2) 1))]
          [output (vector-ref flib (+ (* (f state) 4) (* this 2)))])
      (set! state next)
      output)))

; repeat a string n times (for testing purposes)
(define (repeat-string s n)
  (apply string-append (for/list ([i n]) s)))

; score a flib by calculating it's output, shifting one right
; and counting differences
(define (score-flib flib input)
  (define output (run-flib flib input))
  (- 1.0 (/ (for/sum ([i (- (vector-length input) 1)])
              (abs (- (vector-ref input (+ i 1))
                      (vector-ref output i))))
            (- (vector-length input) 1))))

; mutate a flib by random altering one character
(define (mutate flib)
  (define @ (random (vector-length flib)))
  (for/vector ([i (vector-length flib)]
               [c flib])
    (if (= @ i)
        (if (symbol? c)
            (vector-ref '#(A B C) (random 3))
            (random 2))
        c)))

; breed two flibs by selecting a crossover point and merging input
(define (breed flib1 flib2)
  (define @ (random (vector-length flib1)))
  (for/vector ([i (vector-length flib1)]
               [c1 flib1]
               [c2 flib2])
    (if (< i @) c1 c2)))

; use a genetic algorithm to find a perfect flib
(define (run input
             [pool-size 12]  ; number of flibs to breed
             [kill-off 2]    ; remove the lowest this many flibs each generation
                             ;   refill by breeding two random surviving flibs
             [mutate-% 0.1]) ; mutate this percentage of flibs each generation
    
  ; sort a pool by a given criteria
  (define (sort-pool pool)
    (sort 
     pool
     (lambda (p1 p2) (> (car p1) (car p2)))))
  
  ; iterate until we have a winner
  (let loop ([pool (sort-pool
                    (for/list ([i pool-size]) 
                      (let ([flib (random-flib)])
                        (list (score-flib flib input) flib))))]
             [high-score 0])
    
    ; find a new maximum
    (define new-max (> (caar pool) high-score))
    (define next-high-score (if new-max (caar pool) high-score))
    (define high-flib (cadar pool))
    
    ; display progress
    (when new-max
      (printf "new best flib: ~s scoring ~s\n" high-flib next-high-score))
    
    ; loop (or not)
    (if (< next-high-score 1)
        (loop       
         ; kill off and replace the lowest performing flibs for the new pool
         (sort-pool
          (append
           ; add new flibs by breeding the old ones
           (for/list ([i kill-off]) 
             (let ([flib1 (cadr (list-ref pool (- pool-size kill-off)))]
                   [flib2 (cadr (list-ref pool (- pool-size kill-off)))])
               (let ([flib3 (breed flib1 flib2)])
                 (list (score-flib flib3 input) flib3))))
           ; randomly mutate surviving flibs the given percent of the time
           (for/list ([sflib (drop (reverse pool) kill-off)])
             (if (< (random) mutate-%)
                 (let ([f (mutate (cadr sflib))])
                   (list (score-flib f input) f))
                 sflib))))
         next-high-score)
        
        high-flib)))
    
        
     