#lang racket

; Ordering function for rock/paper/scissors
(define (rps>? p1 p2)
  (or (and (eq? p1 'rock)     (eq? p2 'scissors))
      (and (eq? p1 'scissors) (eq? p2 'paper))
      (and (eq? p1 'paper)    (eq? p2 'rock))))

; Play a game of rock/paper/scissors against a given opponent
(define (play-rps brain)
  (printf "Enter rock, paper, or scissors (quit to exit).\n")
  
  ; Read player input, evaulate it, print response, loop (unless quit)
  (let repl ([wins 0] [rounds 0])
    (printf "> ")
    (define player (read))
    (case player 
      ; Playing a round, run the computer brain
      [(rock paper scissors)
       (define computer (brain))
       (printf " computer chooses ~a" computer)
       (cond
         ; Player beats computer
         [(rps>? player computer)
          (printf ", you win!\n")
          (brain 'lose)
          (repl (+ wins 1) (+ rounds 1))]
         ; Computer beats player
         [(rps>? computer player)
          (printf ", computer wins :(\n")
          (brain 'win)
          (repl wins (+ rounds 1))]
         ; Player and computer tie
         [else
          (printf ", it's a tie\n")
          (brain 'tie)
          (repl (+ wins 1/2) (+ rounds 1))])]
      ; Done player, print stats and exit
      [(quit)
       (printf "You won ~a%. Good job.\n" (round (* 100 (/ wins rounds))))]
      ; Who knows. Maybe if we repeat ourselves it will be helpful
      [else
       (printf "Unknown input.\nEnter rock, paper, or scissors (quit to exit).\n")
       (repl wins rounds)])))

; Play two computers versus each other for n rounds
(define (play-cpu/cpu brain1 brain2 rounds)
  (define wins
    (for/sum ([i (in-range rounds)])
      (define play1 (brain1))
      (define play2 (brain2))
      (cond
        [(rps>? play1 play2)
         (brain1 'win) (brain2 'lose)
         1]
        [(rps>? play2 play1)
         (brain1 'lose) (brain2 'win)
         0]
        [else
         (brain1 'tie) (brain2 'tie)
         1/2])))
  (printf "Player 1 won ~a% of ~a rounds.\n" (round (* 100 (/ wins rounds))) rounds)
  wins)

; Print a grid of brain comparisons
(define (compare-brains brain-names brain-list rounds)
  (printf "<table>\n<tr><td></td><td>~a</td></tr>\n" 
          (string-join brain-names "</td><td>"))
  
  (for ([brian-name (in-list brain-names)]
        [row-brain (in-list brain-list)])
    (printf "<tr><td>~a</td>" brian-name)
    (for ([col-brain (in-list brain-list)])
      (define wins 0)
      (with-output-to-string (thunk (set! wins (play-cpu/cpu row-brain col-brain rounds))))
      (printf "<td>~a</td>" wins))
    (printf "</tr>\n"))
  
  (printf "</table>"))

; ----- ----- ----- ----- ----- 

(define currently-chatty (make-parameter #f))

(define (random-symbol)
  (list-ref '(rock paper scissors) (random 3)))

(define (random-symbol-except not-me)
  (let loop ([maybe-me (random-symbol)])
    (if (eq? maybe-me not-me)
        (loop (random-symbol))
        maybe-me)))

; Just choose at random
(define random-brain
  (case-lambda
    [()
     (random-symbol)]
    [(result)
     (void)]))

; Always choose the same thing
(define (make-stubborn-brain favorite)
  (case-lambda
    [()
     favorite]
    [(result)
     (void)]))

; Tend to be 'streaky', potentially change reponse a given % of the time
(define (make-streaky-brain swap-chance)
  (define current-choice (random-symbol))
  (case-lambda 
    [()
     (when (< (random) swap-chance)
       (when (currently-chatty)
         (printf "It's okay, I didn't like ~a anyways...\n" current-choice))
       (set! current-choice (random-symbol-except current-choice)))
     current-choice]
    [(result)
     (void)]))

; Choose the same thing until you lose, then switch
(define scaredy-brain
  (let ([current-choice (random-symbol)])
    (case-lambda
      [()
       current-choice]
      [(result)
       (case result
         [(lose)
          (when (currently-chatty)
            (printf "Maybe ~a isn't so great after all...\n" current-choice)
            (set! current-choice (random-symbol-except current-choice)))])])))

; Copy the last thing the player chose
(define copycat-brain
  (let ([next-choice (random-symbol)])
    (case-lambda 
      [()
       next-choice]
      [(result)
       (set! next-choice
             (case (list next-choice result)
               [((rock win) (scissors tie) (paper lose)) 'scissors]
               [((rock tie) (scissors lose) (paper win)) 'rock]
               [((rock lose) (scissors win) (paper tie)) 'paper]))])))
