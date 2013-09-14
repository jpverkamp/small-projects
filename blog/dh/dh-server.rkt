#lang racket

(require math/number-theory
         net/base64
         "dh-shared.rkt")

(define current-thread-id (make-parameter 0))

(define (dh-client-thread from-client to-client)
  (λ ()
    ; Store the ID for this client
    (dh-debug-id (format "client thread ~a" (current-thread-id)))
    
    ; Read the shared public values
    (define p (recv from-client))
    (define g (recv from-client))
    
    ; Read the secret from A
    (define A (recv from-client))
    
    ; Generate my own secret number and send it back
    (define b (random RANGE))
    (define B (with-modulus p (modexpt g b)))
    (send B to-client)
    
    ; Generate the shared secret
    (define s (with-modulus p (modexpt A b)))
    
    ; Use it to seed the PRNG
    (random-seed s)
    (let loop ()
      ; Read a message from the client, print it out
      (define message (xor-decode (recv from-client)))
      (printf "client ~a: ~a\n" (current-thread-id) message)
      
      ; Respond politely
      (define response
        (if (equal? message "exit")
            "Good-bye."
            "Hello world."))
      (send (xor-encode response) to-client)
      
      ; Loop unless 'exit'
      (unless (equal? message "exit")
        (loop)))
    
    ; Clean up the connection
    (close-input-port from-client)
    (close-output-port to-client)))

; Create the main server
(define (start-dh-server [port 8000])
  ; Start listening
  (define server (tcp-listen port))
  (printf "server listening\n")
  
  ; Accept each client in their own thread
  (let loop ()
    (define-values (from-client to-client) (tcp-accept server))
    (current-thread-id (+ 1 (current-thread-id)))
    (thread (dh-client-thread from-client to-client))
    (loop))

  ; If something breaks, close down the server
  (tcp-close server))

(start-dh-server)