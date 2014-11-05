#lang racket

(require racket/date
         racket/draw
         (only-in 2htdp/universe 
                  big-bang on-tick to-draw
                  run-movie)
         (only-in 2htdp/image rotate beside above)
         (prefix-in racket: (only-in racket/base time)))

(struct time-data (hour minute second) #:transparent)

(define (time hour minute [second #f])
  (time-data hour minute second))

(define current-size (make-parameter 30))
(define timeless (make-parameter (time 12 0 0)))

; Fix bitmaps so that big-bang / run-movie / etc can render them
(define (fix img) (rotate 0 img))

; Render a clock at the current-size
(define (analog-clock when)
  (match-define (time-data hour minute second) when)
  
  (define size (current-size))
  (define target (make-bitmap size size))
  (define dc (new bitmap-dc% [bitmap target]))
  
  (send dc set-pen "lightgray" 1 'solid)
  (send dc draw-ellipse 0 0 size size)
  (send dc set-pen "black" 1 'solid)
  
  ; Helper to draw a hand given a radius [0, 1.0] and angle 
  ; Angle of 0 is upright, positive angles are clockwise
  (define (draw-hand! r θ)
    (define c (/ size 2))
    (define x (+ c (* 0.5 r size (cos θ))))
    (define y (+ c (* 0.5 r size (sin θ))))
    (send dc draw-line c c x y))
  
  (draw-hand! 0.8 (+ (* pi 1.5) (* 2 pi (/ minute 60))))
  (draw-hand! 0.7 (+ (* pi 1.5) (* 2 pi (/ hour 12))))
  
  (and second
       (begin
         (draw-hand! 0.9 (+ (* pi 1.5) (* 2 pi (/ second 60))))))
  
  target)

; Render a sequence of frames animating a clock spinnging from one time to another
; Hands will always move clockwise but will both move at once (not as a normal clock)
(define (analog-frames α β frames)
  (match-define (time-data α-hour α-minute α-second) α)
  (match-define (time-data β-hour β-minute β-second) β)
  
  (let ([β-hour   (if (>= β-hour α-hour)     β-hour    (+ β-hour 12))]
        [β-minute (if (>= β-minute α-minute) β-minute (+ β-minute 60))]
        [β-second (and α-second β-second
                       (if (>= β-second α-second) β-second (+ β-second 60)))])

    (for/list ([i (in-range frames)])
      (define frame-multiplier (/ i (- frames 1)))
      
      (define hour (+ α-hour (* frame-multiplier (- β-hour α-hour))))
      (define minute (+ α-minute (* frame-multiplier (- β-minute α-minute))))
      
      (define second 
        (and α-second β-second
             (+ α-second (* frame-multiplier (- β-second α-second)))))
      
      (analog-clock (time hour minute second)))))

; 3x3 character 'font' for digits, use the last for hour/minute/second delimiter
(define digits
  (vector "┌─┐│ │└─┘" ; 0
          "─┐  │ ─┴─" ; 1
          " ─┐┌─┘└──" ; 2
          "──┐ ─┤──┘" ; 3
          "  │└─┤  │" ; 4
          "┌─ └─┐──┘" ; 5
          "│  ├─┐└─┘" ; 6
          "──┐  │  │" ; 7 
          "┌─┐├─┤└─┘" ; 8
          "┌─┐└─┤  │" ; 9
          " │     │ " ; delimiter
          " ○     ○ "))

; Render a digital clock using ascii bar graphics
(define (digital-clock when)
  (match-define (time-data hour minute second) when)
  
  (string-join
   (for/list ([line-index (in-range 3)])
     (list->string
      (for*/list ([digit 
                   (in-list
                    (append (list (if (< hour 10) 0 (quotient hour 10))
                                  (remainder hour 10)
                                  10
                                  (if (< minute 10) 0 (quotient minute 10))
                                  (remainder minute 10))
                            (if second
                                (list 10 
                                      (if (< second 10) 0 (quotient second 10))
                                      (remainder second 10))
                                (list))))]
                  [char-index (in-range 3)])
        
        (define str (vector-ref digits digit))
        (define char (string-ref str (+ char-index (* line-index 3))))
        
        char)))
   "\n"))

; Convert the bar images used back into clocks
(define bar->clock
  (hash #\└ (time 3 0 0)
        #\┘ (time 9 0 0)
        #\┼ (time 6 0 30)
        #\─ (time 3 45 45)
        #\┴ (time 6 45 15)
        #\├ (time 3 0 30)
        #\┤ (time 9 30 0)
        #\┬ (time 6 15 45) 
        #\┌ (time 3 30 30)
        #\┐ (time 9 30 30)
        #\│ (time 12 30 30)
        #\○ #f
        #\space #f))

; Make a clock out of clocks!
(define (clock-clock when)
  (define chars (digital-clock when))
  (define empty-frame (analog-clock (timeless)))
  
  (define rows
    (for/list ([line (in-list (string-split chars "\n"))])
      (for/list ([char (in-string line)])
        (cond
          [(hash-ref bar->clock char) => analog-clock]
          [else empty-frame]))))
  
  (apply above (map (curry apply beside) rows)))

; Animate a clock of clocks turning from one time to another
(define (tick-tock α β frames)
  (define α-chars (digital-clock α))
  (define β-chars (digital-clock β))
  
  (define rows*
    (for/list ([α-line (in-list (string-split α-chars "\n"))]
               [β-line (in-list (string-split β-chars "\n"))])
      (for/list ([α-char (in-string α-line)]
                 [β-char (in-string β-line)])
        (analog-frames (or (hash-ref bar->clock α-char) (timeless))
                       (or (hash-ref bar->clock β-char) (timeless))
                       frames))))
  
  (for/list ([i (in-range frames)])
      
    (define rows 
      (for/list ([row (string-split α-chars "\n")]
                 [row-index (in-naturals)])
        (for/list ([char-index (in-range (string-length row))])
          (list-ref (list-ref (list-ref rows* row-index) char-index) i))))
    
    (apply above (map (curry apply beside) rows))))

; Make a tick-tock real time clock
(define (tick-tock-real-time-clock #:12-hour? [12-hour? #f])
  ; Get the current time in hours/minutes/seconds
  (define (now)
    (define date (current-date))
    (time (date-hour date) (date-minute date) #f))
  
  ; Generate a list of frames for the next transition
  ; Note: The big bang clock is supposed to tick 28 times per second
  (define (transition-frames)
    (match-define (time-data hour minute _) (now))
    
    (define next-minute (remainder (+ minute 1) 60))
    (define next-hour (remainder (if (= next-minute 60) (+ hour 1) hour) (if 12-hour? 12 24)))
    
    (tick-tock (time hour minute)
               (time next-hour next-minute)
               56)) ; Note: The big bang clock is supposed to tick 28 times per second
  
  (big-bang (list (now) (transition-frames))
    [on-tick
     (λ (state)
       (match-define (list old-time frames) state)
       (define new-time (now))
       (cond
         ; We've advanced to the new time, jump ahead!
         [(not (equal? old-time new-time))
          (list new-time (transition-frames))]
         ; Freeze if we only have one frame left
         [(null? (rest frames))
          state]
         ; Otherwise, advance one frame
         [else
          (list old-time (rest frames))]))]
    [to-draw
     (λ (state)
       (match-define (list old-time frames) state)
       (fix (first frames)))]))
