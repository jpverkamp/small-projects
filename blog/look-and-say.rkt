#lang racket

(require (except-in 2htdp/image line)
         images/flomap
         plot)
         

; Inspiration: https://www.youtube.com/watch?v=ea7lJkEhytA
; Look-and-Say Numbers (feat John Conway) via Numberphile

; Create a look and see list by combining repeated values into a count and the number
; For example: 111221 becomes 3 1s, 2 2s, 1 1 => 312211
(define (look-and-say ls)
  (apply 
   append
   (let count ([ls (rest ls)] [i 1] [v (first ls)])
     (cond
       [(null? ls)
        (list (list i v))]
       [(equal? (first ls) v)
        (count (rest ls) (+ i 1) v)]
       [else
        (list* (list i v) (count (rest ls) 1 (first ls)))]))))

; Create a look and say sequence with regular expressions instead of lists
(define (look-and-say/regex str)
  (regexp-replace* 
   #px"(.)(\\1*)" 
   str 
   (λ (match block repeat) (~a (string-length match) block))))

; Make an infinite sequence that generates look-and-see lists
; Use the current look-and-say list itself as both the key and value
(define (in-look-and-say [ls '(1)])
  (make-do-sequence
   (thunk 
    (values
     identity       ; Current
     look-and-say   ; Next
     ls             ; Initial
     (const #t)     ; Continue from this key/value/pair
     (const #t)
     (const #t)))))

; Take the first chunk off of a sequence
(define (look-and-say* ls i)
  (for/list ([ls (in-look-and-say ls)]
             [_  (in-range i)])
    ls))

; Get the nth look-and-say pattern for a given initial state
(define (nth-look-and-say ls n)
  (for/fold ([ls ls]) ([i (in-range (- n 1))])
    (look-and-say ls)))

; Apply a function to the look and say sequence at each iteration
(define (plot-look-and-say f ls bound)
  (plot (lines (for/list ([ls (in-look-and-say ls)]
                          [i  (in-range bound)])
                 (vector i (f ls))))))

; Render a look and say sequence to a bitmap, stretching rows to the entire width
; Note: Values are clamped to between 0.0 and 1.0 before conversion. *rolls eyes*
(define (render-look-and-say ls bound)
  ; Precalculate the image data; figuring out what dimenions will we need from that
  (define ls* (look-and-say* ls bound))
  (define height (length ls*))
  (define width (length (last ls*)))
  
  ; Precalculated list of colors that are defined to be more visually distinct
  (define colors 
    '#(#(1.00 0.70 0.00) #(0.50 0.24 0.46) #(1.00 0.41 0.00) #(0.65 0.74 0.84) #(0.75 0.00 0.12)
       #(0.80 0.63 0.38) #(0.50 0.44 0.40) #(0.00 0.49 0.20) #(0.96 0.46 0.55) #(0.00 0.32 0.54) 
       #(1.00 0.48 0.36) #(0.32 0.21 0.48) #(1.00 0.55 0.00) #(0.70 0.16 0.32) #(0.95 0.78 0.00) 
       #(0.50 0.09 0.05) #(0.57 0.66 0.00) #(0.35 0.20 0.08) #(0.94 0.23 0.07) #(0.14 0.17 0.09)))

  ; Generate the image, three channels are RGB
  ; Note: 4 channels is ARGB, not RGBA *rolls eyes again*
  (flomap->bitmap 
   (build-flomap* 
    3 width height
    (λ (x y)
      ; Pull out the correct row for the data, normalize entries to 'stretch' over the entire row
      (define row (list-ref ls* y))
      (define row-width (length row))
      (define index (quotient (* x row-width) width))
      (vector-ref colors (list-ref row index))))))

; Rescale an image to a given size
; I'm sure there's a function for this, I just cannot find it
(define (scale-to width height bitmap)
  (define x-scale (/ width (send bitmap get-width)))
  (define y-scale (/ height (send bitmap get-height)))
  (scale/xy x-scale y-scale bitmap))
