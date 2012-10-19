; expand l systems
(define-syntax l-system
  (syntax-rules (->)
    [(l-system init rules ...)
     (lambda (n)

       ; recursive nested map
       (define (deep-map proc ls)
         (cond
           [(null? ls) '()]
           [(pair? (car ls))
            (cons (deep-map proc (car ls))
                  (deep-map proc (cdr ls)))]
           [else
            (cons (proc (car ls))
                  (deep-map proc (cdr ls)))]))

       ; expand an l-system
       (define (expand n ls)
         (if (zero? n)
             ls
             (apply append
               (deep-map
                 (lambda (x)
                   (let ([result (assoc x '(rules ...))])
                     (if result
                         (expand (sub1 n) (cddr result))
                         (list x))))
                 ls))))

       ; start with init
       (expand n '(init)))]))

#| examples |#

(define algae  ; an l-system is a one variable function (# of iterations)
  (l-system a  ; create a new l-system starting at a
    [a -> a b] ; bind a set of rules, each expanding a single non-terminal
    [b -> a]))

(define fern
  (l-system x
    [f -> f f]
    [x -> f - [[x] + x] + f [+ f x] - x]))