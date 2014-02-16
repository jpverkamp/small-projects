#lang racket

(require "util.rkt"
         sha
         (only-in file/sha1 bytes->hex-string))

; Mine for the lowest SHA-512 we can find
(define (mine)
  (define alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890")
  (define hashes-tried 0)
  (define start-time (current-inexact-milliseconds))
  
  ; Store best values
  (define best-str #f)
  (define best-hash #f)
    
  ; Generate random values one at a time
    (let loop ()
      (define str   (random-string 16))
      (define bytes (string->bytes/utf-8 str))
      (define hash  (bytes->hex-string (sha512 bytes)))
      
      (set! hashes-tried (+ 1 hashes-tried))
      (when (or (not best-hash) (string<? hash best-hash))
        (set! best-str str)
        (set! best-hash hash)
        (print-timing 'main hashes-tried (/ (- (current-inexact-milliseconds) start-time) 1000) best-str best-hash))
      
      (loop)))

; Run from the command line if a number of threads is specified
(module+ main
  (mine))