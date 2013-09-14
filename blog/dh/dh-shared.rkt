#lang racket

(require net/base64)

(provide (all-defined-out))

; Maximum value for randomly generated primes
(define RANGE (expt 2 16))

; Set to print debugging messages on send/recv
(define dh-debug-mode (make-parameter #f))
(define dh-debug-id   (make-parameter (random RANGE)))

; Receive a message from a port, must be Racket readable
(define (recv from)
  (define value (read from))
  (when (dh-debug-mode)
    (printf "~a recv (~a): ~a\n" (dh-debug-id) from value))
  value)

; Send a value to a port; write a newline and flush to force send
(define (send value to)
  (when (dh-debug-mode)
    (printf "~a send (~a): ~a\n" (dh-debug-id) to value))
  (write value to)
  (newline to)
  (flush-output to))

; Use XOR and Base64 to encode a message, depends on random seed
(define (xor-encode message)
  (base64-encode
   (string->bytes/utf-8
    (list->string
     (for/list ([c (in-string message)])
       (integer->char (bitwise-xor (random 256) (char->integer c))))))
   #""))

; Decode a message encoded with the previous, depends on random seed
(define (xor-decode response)
  (list->string
   (for/list ([c (in-string (bytes->string/utf-8 (base64-decode response)))])
     (integer->char (bitwise-xor (random 256) (char->integer c))))))
