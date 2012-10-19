#lang racket

(provide
 make-queue
 queue-empty?
 queue-push!
 queue-pop!)

; opaque structure for the queue
(define-struct :queue: (values head tail) #:mutable)

; create a new queue
(define (make-queue) (make-:queue: (make-hasheq) 0 0))

; test if the queue is empty
(define (queue-empty? q)
  (= (:queue:-head q) (:queue:-tail q)))

; push an item onto the queue
(define (queue-push! q v)
  (hash-set! (:queue:-values q) (:queue:-tail q) v)
  (set-:queue:-tail! q (+ 1 (:queue:-tail q))))

; pop an item from the queue and return
(define (queue-pop! q)
  (let ([v (hash-ref (:queue:-values q) (:queue:-head q))])
    (hash-remove! (:queue:-values q) (:queue:-head q))
    (set-:queue:-head! q (+ 1 (:queue:-head q)))
    v))
