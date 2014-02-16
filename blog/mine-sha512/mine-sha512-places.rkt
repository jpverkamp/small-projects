#lang racket

(require "util.rkt"
         sha
         (only-in file/sha1 bytes->hex-string))

; Run multiple mining places in parallel and sync between them
(define (mine-all thread-count)
  ; Global best values
  (define start-time (current-inexact-milliseconds))
  (define best-str #f)
  (define best-hash #f)
  
  ; Create places
  (define places
    (for/list ([i (in-range thread-count)])
      (mine i)))
  
  ; Loop through those, getting their best values in time
  (let loop ()
    (for ([id (in-naturals)]
          [p (in-list places)])
      (define hashes-tried (place-channel-get p))
      (define str (place-channel-get p))
      (define hash (place-channel-get p))
      
      (when (or (not best-hash) (and hash (string<? hash best-hash)))
        (define estimated-hashes (* hashes-tried thread-count))
        (set! best-str str)
        (set! best-hash hash)
        (print-timing id estimated-hashes (/ (- (current-inexact-milliseconds) start-time) 1000) best-str best-hash)))
    (loop)))

; Create a place that will mine for new low hashes
(define (mine id)
  (place me
    ; Local best values
    (define hashes-tried 0)
    (define best-str #f)
    (define best-hash #f)
    
    ; Thread to periodically send back new best values
    (thread
      (thunk
        (let loop ()
          (place-channel-put me hashes-tried)
          (place-channel-put me best-str)
          (place-channel-put me best-hash)
          (sleep (* (random) 10.0))
          (loop))))
    
    ; Look for new values
    (let loop ()
      (define str   (random-string 16))
      (define bytes (string->bytes/utf-8 str))
      (define hash  (bytes->hex-string (sha512 bytes)))
      (set! hashes-tried (+ 1 hashes-tried))
      
      (when (or (not best-hash) (string<? hash best-hash))
        (set! best-str str)
        (set! best-hash hash))
      (loop))))

; Run from the command line if a number of threads is specified
(module+ main
  (when (= 1 (vector-length (current-command-line-arguments)))
    (mine-all (string->number (vector-ref (current-command-line-arguments) 0)))))