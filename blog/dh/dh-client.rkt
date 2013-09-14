#lang racket

(require math/number-theory
         "dh-shared.rkt")

(define (start-dh-client [server-host "localhost"] [server-port 8000])
  ; Generate an ID for this client
  (dh-debug-id (format "client ~a" (random RANGE)))
  
  ; Connect to the DH server
  (define-values (from-server to-server) 
    (tcp-connect server-host server-port))
  
  ; Generate and share the public information
  (define p (random-prime RANGE))
  (define g (first (shuffle (primitive-roots p))))
  (send p to-server)
  (send g to-server)
  
  ; Generate my secret integer, send the shared secret to the server
  (define a (random RANGE))
  (define A (with-modulus p (modexpt g a)))
  (send A to-server)
  
  ; Get their half of the shared secret
  (define B (recv from-server))
  
  ; Calculate the secret
  (define s (with-modulus p (modexpt B a)))
  
  ; Use it to seed the PRNG
  (random-seed s)
  (let loop ()
    ; Ask the user for input, encode it, send it
    (printf "client: ")
    (define message (read-line))
    (send (xor-encode message) to-server)
  
    ; Read a response from the server, print it out
    (define response (xor-decode (recv from-server)))
    (printf "server: ~a\n" response)
    
    ; Loop unless 'exit'
    (unless (equal? message "exit")
      (loop)))

  ; Close down the connection
  (close-input-port from-server)
  (close-output-port to-server))

(start-dh-client)