#lang racket

(require "util.rkt"
         sha
         (only-in file/sha1 bytes->hex-string))

; Store best values
(define hashes-tried 0)
(define best-str #f)
(define best-hash #f)
(define start-time (current-inexact-milliseconds))

; Mine with multiple threads concurrently
(define (mine-all thread-count)
  (define threads
    (for/list ([i (in-range thread-count)])
      (thread (thunk (mine i)))))
  (map thread-wait threads))

; Mine for the lowest SHA-512 we can find
(define (mine id)
  ; Generate random values one at a time
  (let loop ()
    (define str   (random-string 16))
    (define bytes (string->bytes/utf-8 str))
    (define hash  (bytes->hex-string (sha512 bytes)))
    
    (set! hashes-tried (+ 1 hashes-tried))
    (when (or (not best-hash) (string<? hash best-hash))
      (set! best-str str)
      (set! best-hash hash)
      (print-timing id hashes-tried (/ (- (current-inexact-milliseconds) start-time) 1000) best-str best-hash))
    
    (loop)))

; Run from the command line if a number of threads is specified
(module+ main
  (when (= 1 (vector-length (current-command-line-arguments)))
    (mine-all (string->number (vector-ref (current-command-line-arguments) 0)))))