#lang racket

(require images/flomap)

; Get the maximum path length; useful for making gradients
(define (size->path-length size)
  (inexact->exact (floor (/ (log size) (log 2)))))

(define current-size     (make-parameter 64))
(define current-coloring (make-parameter (thunk* '#(1 1 1))))
(define current-mode     (make-parameter 'short))

; Generate a fractal by matching a recursive path into an image
(define (regex-fractal regex)
  (flomap->bitmap
   (build-flomap*
    3 (current-size) (current-size)
    (λ (x y)
      (let loop ([t 0] [l 0] [s (current-size)] [path ""])
        (cond
          ; If we're short circuiting, check the regex; if it matches, apply the coloring
          [(and (eq? (current-mode) 'short)
                (regexp-match regex path)) => (current-coloring)]
          ; If we're at the last level, always apply the coloring function
          ; If it fails, always return black 
          [(<= s 1)
           (cond
             [(regexp-match regex path) => (current-coloring)]
             [else '#(0 0 0)])]
          ; Otherwise, divide the region into four subregions and recur into the proper one
          [else
           (define s/2 (quotient s 2))
           (define x-mid (+ l s/2))
           (define y-mid (+ t s/2))
           (loop 
            (if (< y y-mid) t y-mid)
            (if (< x x-mid) l x-mid)
            s/2
            (~a path 
                (match (list (< y y-mid) (< x x-mid))
                  ['(#t #t) 2]
                  ['(#t #f) 1]
                  ['(#f #t) 3]
                  ['(#f #f) 4])))]))))))

; ----- Examples -----

(define all-colorings
  (hash
   ; Basic coloring, all matching pixels are white
   'default      
   (thunk* '#(1 1 1))
   ; Make a gradient based on the length of the capturing groups
   ; If the group is full length, use white
   ; 1 group = green only
   ; 2 groups = green then red
   ; 3 groups = RGB
   'by-length
   (λ (m)
     (define l (string-length (car m)))
     (define p (size->path-length (current-size)))
     (if (= l p)
         '#(1 1 1)
         (vector
          (if (>= (length m) 3) (/ (string-length (list-ref m 2)) p) 0)
          (if (>= (length m) 2) (/ (string-length (list-ref m 1)) p) 0)
          (if (>= (length m) 4) (/ (string-length (list-ref m 3)) p) 0))))
   ; Color based on the most common number
   'common-voting
   (λ (m)
     (define-values (which _)
       (for/fold ([max-i #f] [max-v 0])
                 ([i (in-range 1 5)])
         (define v (count 
                    (λ (c) (= i (string->number (string c))))
                    (string->list (car m))))
         (if (> v max-v)
             (values i v)
             (values max-i max-v))))
     
     (define l (string-length (car m)))
     (define p (size->path-length (current-size)))
     (define g (/ l p))
                          
     (case which
       [(1)  (vector g g g)]
       [(2)  (vector g 0 0)]
       [(3)  (vector 0 g 0)]
       [(4)  (vector 0 0 g)]
       [else (vector 0 0 0)]))
   ))

(define all-regexes
  (hash
   'sierpinski        #px".*1.*"
   'four-corners      #px"((.)(\\2*))"
   'left-right        #px"(1|2)"
   'jagged            #px"(12)"
   'double-sierpinski #px"(1.*2|2.*1)"
   'ones-then-twos    #px"^[34]*[134]*[34]*[234]*[34]*$"
   'ascending         #px"^1*2*3*4*$"
   'no-one            #px"^[^1]*$"
   'even-sum          #px"^(2|4|[13][24]*[13])*$"
   'odd-sum           #px"^(2|4|[13][24]*[13])*[13](2|4|[13][24]*[13])*$"
   'ones-at-the-end   #px"1$"
   ; comments @ https://www.reddit.com/r/dailyprogrammer/comments/2fkh8u/
   'curls             #px"[13][24][^1][^2][^3][^4]"
   'self-similar      #px"(.)\\1..\\1"
   'thumbsup          #px".*1*2[^24]34*(.*)"
   'boxes             #px"(?:13|31)(.*)"
   'spiky             #px"(12|23|34)"
   'sierpinski-nest   #px"4[^4][^4]2(.*)"
   ; https://imgur.com/a/QWMGi
   'boxes             #px"(13|31|24|42)"
   'boxes-remainder   #px"(?:13|31|24|42)(.*)"
   'squared-carpet    #px"(13|31)"
   'outlined          #px"(1[124]|2[14]|5[12|31])*"
   'figure-eights     #px"(?:..)*(?:[13][13]|[24][24])((?:..)*)"
   'scanlines         #px"^[13]*[24]*$"
   ))

(define (demo folder sizes)
  (make-directory* folder) ; NOP if folder exists
  (for* ([(regex-name    regex)    (in-hash all-regexes)]
         [mode                     (in-list '(short full))]
         [(coloring-name coloring) (in-hash all-colorings)]
         [size                     (in-list (if (list? sizes) sizes (list sizes)))])
    (parameterize ([current-size size]
                   [current-coloring coloring]
                   [current-mode mode])
      
      (define path 
        (build-path folder 
                    (~a regex-name "_" mode "_" coloring-name "_" size ".png")))

      (cond
        [(file-exists? path)
         (printf "~a (exists)\n" path)]
        [else
         (displayln path)
         (flush-output)
         (define img (regex-fractal regex))
         (send img save-file path 'png)]))))
