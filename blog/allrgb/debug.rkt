#lang racket

(provide debug)

; Keep track of starting time for each key
(define starting-times (make-hash))
(define last-percents (make-hash))
(define last-print-length 0)

; Format time nicely
(define (format-time ms)
  (let* ([seconds (inexact->exact (floor (/ ms 1000)))]
         [hours (quotient seconds (* 60 60))]
         [seconds (- seconds (* hours 60 60))]
         [minutes (quotient seconds 60)]
         [seconds (- seconds (* minutes 60))])
    (~a hours
        ":"
        (~a minutes #:width 2 #:align 'right #:pad-string "0")
        ":"
        (~a seconds #:width 2 #:align 'right #:pad-string "0"))))

; Print out a debug indicator and estimated time remaining
(define (debug key progress total)
  ; Set the starting timestamp if this is a new key
  (define now (current-inexact-milliseconds))
  (when (not (hash-has-key? starting-times key))
    (hash-set! starting-times key now))
  
  ; Print if the percent complete has changed
  (define current-percent (quotient (* 100 progress) total))
  (when (or (not (hash-has-key? last-percents key))
            (not (= (hash-ref last-percents key) current-percent)))
    
    (define ms-elapsed (- now (hash-ref starting-times key)))
    (define ms-total (if (zero? progress) 0 (* total (/ ms-elapsed progress))))
    
    (printf "~a\r" (make-string last-print-length #\space))
    (printf "~a ~a% complete, ~a so far, ~a remaining\r"
            key
            current-percent
            (format-time ms-elapsed)
            (format-time (- ms-total ms-elapsed)))
    
    (flush-output)
    
    ; Update the last percent to avoid reprinting
    (hash-set! last-percents key current-percent)))