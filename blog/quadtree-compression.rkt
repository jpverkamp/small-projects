#lang racket

(require (only-in 2htdp/image empty-scene place-image)
         2htdp/universe
         images/flomap
         math/statistics
         racket/draw)

(struct quadtree (top-left top-right bottom-left bottom-right) #:transparent)
(struct region (top left width height) #:transparent)

(struct qtnode (region color error) #:transparent)
(struct qtimage (flomap nodes))

; Map a function over the nodes in a quadtree
(define (quadtree-map f qt)
  (cond
    [(quadtree? qt)
     (quadtree
      (quadtree-map f (quadtree-top-left qt))
      (quadtree-map f (quadtree-top-right qt))
      (quadtree-map f (quadtree-bottom-left qt))
      (quadtree-map f (quadtree-bottom-right qt)))]
    [else (f qt)]))

; Reduce all nodes in a quadtree
(define (quadtree-reduce f qt)
  (cond
    [(quadtree? qt)
     (f (quadtree-reduce f (quadtree-top-left qt))
        (quadtree-reduce f (quadtree-top-right qt))
        (quadtree-reduce f (quadtree-bottom-left qt))
        (quadtree-reduce f (quadtree-bottom-right qt)))]
    [else qt]))

; Recur to a given point within a quadtree
(define (quadtree-ref qt width height x y #:return-region [return-region? #f])
  (let loop ([qt qt] [r (region 0 0 width height)])
    (cond 
      [(quadtree? qt)
       (match-define (region top left width height) r)
       (define x-mid (+ left (quotient width 2)))
       (define y-mid (+ top  (quotient height 2)))
       (match (list (if (< y y-mid) 'top 'bottom)
                    (if (< x x-mid) 'left 'right))
         ['(top    left)  (loop (quadtree-top-left     qt) (region top   left  (quotient width 2) (quotient height 2)))]
         ['(bottom left)  (loop (quadtree-bottom-left  qt) (region y-mid left  (quotient width 2) (quotient height 2)))]
         ['(top    right) (loop (quadtree-top-right    qt) (region top   x-mid (quotient width 2) (quotient height 2)))]
         ['(bottom right) (loop (quadtree-bottom-right qt) (region y-mid x-mid (quotient width 2) (quotient height 2)))])]
      [return-region? r]
      [else qt])))

; Render a tree where each node is either a quadtree or a vector (color)
(define (render-quadtree qt width height)
  (flomap->bitmap
   (build-flomap* 
    4 width height
    (λ (x y) (quadtree-ref qt width height x y)))))

; Calculate the average color within a region
(define (region-node fm r)
  (match-define (region top left width height) r)
  
  (define med
    (for/vector ([k (in-range 4)])
      (with-handlers ([exn? (λ _ (flomap-ref fm k left top))])
        (median < (for/list ([x (in-range left (+ left width))]
                             [y (in-range top (+ top height))])
                    (flomap-ref fm k x y))))))
  
  (define err
    (for*/sum ([k (in-range 4)]
               [x (in-range left (+ left width))]
               [y (in-range top (+ top height))])
      (abs (- (flomap-ref fm k x y) (vector-ref med k)))))
  
  (qtnode r med err))

; Load an image in preparation for quadtree splitting
(define (load-image path)
  (define fm (bitmap->flomap (read-bitmap path)))
  (define-values (width height) (flomap-size fm))
  (define r (region 0 0 width height))
  (define node (region-node fm r))
  (qtimage fm node))

; Given an image, split the region with the highest error
(define (split-image img)
  ; Find the maximum error
  (define max-error-node
    (quadtree-reduce 
     (λ ns (car (sort ns (λ (na nb) (> (qtnode-error na) (qtnode-error nb))))))
     (qtimage-nodes img)))
  
  ; Replace nodes with that error with their child nodes, calculating those errors
  (define fm (qtimage-flomap img))
  (qtimage 
   fm
   (quadtree-map 
    (λ (node)
      (cond
        [(eq? node max-error-node)
         (match-define (region t l w h) (qtnode-region node))
         (define w/2 (quotient w 2))
         (define h/2 (quotient h 2))
         (quadtree
          (let ([r (region t         l         w/2 h/2)]) (region-node fm r))
          (let ([r (region t         (+ l w/2) w/2 h/2)]) (region-node fm r))
          (let ([r (region (+ t h/2) l         w/2 h/2)]) (region-node fm r))
          (let ([r (region (+ t h/2) (+ l w/2) w/2 h/2)]) (region-node fm r)))]
        [else node]))
    (qtimage-nodes img))))

; Render an image
(define (render-image img)
  (define-values (width height) (flomap-size (qtimage-flomap img)))
  (render-quadtree (quadtree-map qtnode-color (qtimage-nodes img)) width height))

; Progressively compress an image
(define (compress img)
  (define-values (width height) (flomap-size (qtimage-flomap img)))
  (define base-scene (empty-scene width height))
  (big-bang img
    [on-tick split-image]
    [to-draw (λ (img) (place-image (render-image img) (/ width 2) (/ height 2) base-scene))]
    [record? #t]))
