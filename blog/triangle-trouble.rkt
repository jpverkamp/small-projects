#lang racket

; Represent a triangle as three angles and three sides
; Angles can be in either degrees or radians so long as all three are the same
; Any value is either numeric or #f if it is currently unknown
(struct triangle (∠ɑ ∠β ∠γ a b c) #:transparent)

; Convert all angles in a triangle from radians to degrees
(define (triangle-radians->degrees t)
  (define (r->d ∠) (and ∠ (radians->degrees ∠)))
  (match t
    [(triangle ∠ɑ ∠β ∠γ a b c)
     (triangle (r->d ∠ɑ) (r->d ∠β) (r->d ∠γ) a b c)]
    [#f #f]))

; Convert all angles in a triangle from degrees to radians
(define (triangle-degrees->radians t)
  (define (d->r ∠) (and ∠ (degrees->radians ∠)))
  (match t
    [(triangle ∠ɑ ∠β ∠γ a b c)
     (triangle (d->r ∠ɑ) (d->r ∠β) (d->r ∠γ) a b c)]
    [#f #f]))

; If all six fields are numeric, we've solved the triangle
(define (solved? t)
  (andmap number? (cdr (vector->list (struct->vector t)))))

; Given some of the sides/angles of a triangle try to solve for the rest
(define (solve/radians t)
  (define tried '())
  (let loop ([t t])
    (set! tried (cons t tried))
    (match t
      ; We have all three sides and angles, return
      [(? solved?) t]
      ; We've already tried this solution, backtrack
      [(? (curryr member (cdr tried)))
       #f]
      ; Two angles, solve for the third
      [(triangle (? number? ∠ɑ) (? number? ∠β) #f a b c)
       (loop (triangle ∠ɑ ∠β (- pi ∠ɑ ∠β) a b c))]
      ; Sine rule 1: Matching side/angle + angle, solve for missing side
      [(triangle (? number? ∠ɑ) (? number? ∠β) ∠γ
                 (? number?  a)             #f   c)
       (loop (triangle ∠ɑ ∠β ∠γ a (/ (* a (sin ∠β)) (sin ∠ɑ)) c))]
      ; Sine rule 2: Matching side/angle + side, solve for missing angle
      [(triangle (? number? ∠ɑ)            #f ∠γ
                 (? number?  a) (? number?  b) c)
       (loop (triangle ∠ɑ (asin (/ (* b (sin ∠ɑ)) a)) ∠γ a b c))]
      ; Cosine rule 1: Angle and the other two sides, solve for third side
      [(triangle (? number? ∠ɑ)           ∠β             ∠γ
                 #f             (? number?  b) (? number?  c))
       (loop (triangle ∠ɑ ∠β ∠γ (sqrt (+ (sqr b) (sqr c) (- (* b c (cos ∠ɑ)))))))]
      ; Cosine rule 2: Three sides, solve for one angle
      [(triangle #f                        ∠β             ∠γ
                 (? number?  a) (? number?  b) (? number?  c))
       (loop (triangle (acos (/ (+ (sqr b) (sqr c) (- (sqr a))) (* 2 b c))) ∠β ∠γ a b c))]
      ; Try another ordering
      [(triangle ∠ɑ ∠β ∠γ a b c)
       (or (loop (triangle ∠β ∠γ ∠ɑ b c a))
           (loop (triangle ∠β ∠ɑ ∠γ b a c)))])))

; Given some of the sides/angles of a triangle try to solve for the rest
(define (solve/degrees t)
  (triangle-radians->degrees (solve/radians (triangle-degrees->radians t))))
