#lang racket

(require net/base64)

(provide (all-defined-out))

(define RANGE (expt 2 16))

(define dh-debug-mode (make-parameter #f))
(define dh-debug-id   (make-parameter (random RANGE)))

(define (recv from)
  (define value (read from))
  (when (dh-debug-mode)
    (printf "~a recv (~a): ~a\n" (dh-debug-id) from value))
  value)

(define (send value to)
  (when (dh-debug-mode)
    (printf "~a send (~a): ~a\n" (dh-debug-id) to value))
  (write value to)
  (newline to)
  (flush-output to))

(define (xor-encode message)
  (base64-encode
   (string->bytes/utf-8
    (list->string
     (for/list ([c (in-string message)])
       (integer->char (bitwise-xor (random 256) (char->integer c))))))
   #""))

(define (xor-decode response)
  (list->string
   (for/list ([c (in-string (bytes->string/utf-8 (base64-decode response)))])
     (integer->char (bitwise-xor (random 256) (char->integer c))))))