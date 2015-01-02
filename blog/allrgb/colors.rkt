#lang racket

(provide list-rgb-producers
         get-rgb-producer
         in-rgb-producers
         rgb-distance
         rgb<?)

(require racket/generator)

; Distance between two flvectors
(define (rgb-distance c1 c2)
  (sqrt 
   (for/sum ([v1 (in-vector c1)] [v2 (in-vector c2)])
     (sqr (- v1 v2)))))

; Compare two colors, order by red then green then blue
(define (rgb<? c1 c2)
  (for/first ([v1 (in-vector c1)]
              [v2 (in-vector c2)] 
              #:when (not (= v1 v2)))
    (< v1 v2)))

; Interface to export all known rgb producers 
(define rgb-producers (make-hash))

(define (list-rgb-producers) (hash-keys rgb-producers))
(define (get-rgb-producer name) (hash-ref rgb-producers name))
(define (in-rgb-producers) (in-hash rgb-producers))

(define-syntax-rule (define-rgb-producer (name count) body ...)
  (let ()
    (define (name count) body ...)
    (hash-set! rgb-producers 'name name)))

; ===== ===== ===== ===== =====

(define 256^3 (expt 256 3))

(define (->gray-code i)
  (bitwise-xor (arithmetic-shift i 1) i))

; Convert an integer [0, 256^3) into an ARGB flvector
(define (->rgb n)
  (define (->1.0 n) (/ n 256.0))
  (vector 1.0
          (->1.0 (bitwise-and (arithmetic-shift n -16) 255))
          (->1.0 (bitwise-and (arithmetic-shift n -8) 255))
          (->1.0 (bitwise-and (arithmetic-shift n -0) 255))))

; Move evenly through the RGB color space
(define-rgb-producer (sequential-noskip count)
  (generator ()
    (for ([i (in-range 256^3)])
      (yield (->rgb i)))))

(define-rgb-producer (sequential count)
  (define increment (quotient 256^3 count))
  (generator ()
    (for ([i (in-range 0 256^3 increment)])
      (yield (->rgb i)))))

; Generate codes using gray codes 
(define-rgb-producer (gray-code-noskip count)
  (generator ()
    (for ([i (in-range 256^3)])
      (yield (->rgb (->gray-code i))))))

(define-rgb-producer (gray-code count)
  (define increment (quotient 256^3 count))
  (generator ()
    (for ([i (in-range 0 256^3 increment)])
      (yield (->rgb (->gray-code i))))))

; Randomly generate colors (regenerating on duplicates)
(define-rgb-producer (randomly count)
  (define used (make-hasheq))
  (λ ()
    (for*/first ([_ (in-naturals)]
                 [new-color (in-value (random 256^3))]
                 #:when (not (hash-has-key? used new-color)))
      (hash-set! used new-color #t)
      (->rgb new-color))))

; Cascade through red, then green, then blue
(define-rgb-producer (cascade count)
  (define increment (inexact->exact (floor (/ 256 (expt count 1/3)))))
  (generator ()
    (for* ([r (in-range 0 256 increment)]
           [g (in-range 0 256 increment)]
           [b (in-range 0 256 increment)])
      (yield (->rgb (+ r (* 256 g) (* 256 256 b)))))))

(define-rgb-producer (cascade-gray-code count)
  (define increment (inexact->exact (floor (/ 256 (expt count 1/3)))))
  (generator ()
    (for* ([r (in-range 0 256 increment)]
           [g (in-range 0 256 increment)]
           [b (in-range 0 256 increment)])
      (yield (->rgb (+ (->gray-code r) 
                       (* 256 (->gray-code g)) 
                       (* 256 256 (->gray-code b))))))))

; ===== ===== ===== ===== =====

(module+ test
  (require images/flomap)
  
  (define (verify f size)
    (define count (* size size))
    (define unique-colors
      (for/set ([i (in-range count)]
                [c (in-producer (f count))])
        c))
    
    (when (not (= count (set-count unique-colors)))
      (error (object-name f)
             (format "not enough colors, got ~a expected ~a" (set-count unique-colors) count))))
  
  (define (test f size)
    (verify f size)
    
    (define count (* size size))
    (define colors
      (for/vector ([i (in-range count)]
                   [c (in-producer (f count))])
        c))
    
    (define img
      (flomap->bitmap
       (build-flomap*
        4 size size
        (λ (x y)
          (vector-ref colors (+ x (* y size)))))))
    
    (define channels
      (for/list ([channel (in-range 4)])
        (for/sum ([color (in-vector colors)])
          (vector-ref color channel))))
    
    (list img channels))
  
  (for/list ([(name function) (in-hash rgb-producers)])
    (printf "testing ~a\n" name)
    (list name (test function 200))))

