#lang racket

(provide magnitude
         dot-product
         cosine-similarity)

; Calculate the magnitude of a vector
(define (magnitude v)
  (sqrt (for/sum ([vi (in-vector v)]) (* vi vi))))

; Calculate the dot product of two vectors
(define (dot-product a b)
  (for/sum ([ai (in-vector a)]
            [bi (in-vector b)])
    (* ai bi)))

; Calculate the similarity between two vectors
; If inputs are hashes, merge them before calculating similarity
(define (cosine-similarity a b)
  (cond
    [(and (hash? a) (hash? b))
     (define keys
       (set->list (set-union (list->set (hash-keys a))
                             (list->set (hash-keys b)))))
     (cosine-similarity
      (for/vector ([k (in-list keys)]) (hash-ref a k 0))
      (for/vector ([k (in-list keys)]) (hash-ref b k 0)))]
    [else
     (define cossim (acos (/ (dot-product a b) (* (magnitude a) (magnitude b)))))
     (- 1.0 (/ (abs cossim) (/ pi 2)))]))

