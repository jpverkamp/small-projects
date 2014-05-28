#lang racket

(require images/flomap
         math/flonum
         racket/draw)

; Wrapper to read directly to a flomap
(define (read-flomap path)
  (bitmap->flomap (read-bitmap path)))

; Convert a flomap into a dot matrix string, using Braille for the pixels
(define (flomap->braille fm #:threshold [threshold 0.75])
  ; Braille characters are two dots wide and four tall
  (define-values (width height) (flomap-size fm))
  (define b-width  (ceiling (/ width 2)))
  (define b-height (ceiling (/ height 4)))
  
  ; Return if the grayscale (simple average) at a point is greater than a threshold
  ; flomap-ref* is already 'safe' in that out of bounds pixels are always 0
  (define (bit x y)
    (define c (flomap-ref* fm x y))
    (define g (/ (flvector-sum c) (flvector-length c)))
    (> g threshold))
  
  ; Load each line directly to a string, join with newlines
  (string-join
   (for/list ([b-y (in-range b-height)])
     (list->string
      (for/list ([b-x (in-range b-width)])
        (integer->char 
         ; The Braille unicode range is #x2800 - #x28FF, where each dot is one of 8 bits
         (+ #x2800
            ; Because Braille was originally only 6 dots, the order of bits is:
            ; 1 4 
            ; 2 5
            ; 3 6
            ; 7 8
            (for/sum ([xΔ  (in-list '(0 0 0 1  1  1  0   1))]
                      [yΔ  (in-list '(0 1 2 0  1  2  3   3))]
                      [mul (in-list '(1 2 4 8 16 32 64 128))])
              (* mul (if (bit (+ (* 2 b-x) xΔ) (+ (* 4 b-y) yΔ)) 1 0))))))))
   "\n"))
