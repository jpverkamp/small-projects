#lang racket

(define (match-brackets [matching #f])
  (define pairs '((#\( . #\)) (#\{ . #\}) (#\[ . #\])))
  (define c (read-char))
  (cond
    [(eof-object? c)            (not matching)]
    [(assoc c pairs)            => (λ (pair) (and (match-brackets (cdr pair)) 
                                                  (match-brackets matching)))]
    [(eq? c matching)           #t]
    [(member c (map cdr pairs)) #f]
    [else                       (match-brackets matching)]))

(for ([s (in-list '("{}" "[]" "()" "a(b)c" "abc[d]" "a(b)c{d[e]}"))])
  (printf "~a, matches? ~a\n" s (with-input-from-string s match-brackets)))

(for ([s (in-list '("{]" "(]" "a(b]c" "abc[d}" "a(b)c{d[e}]" "("))])
  (printf "~a, matches? ~a\n" s (with-input-from-string s match-brackets)))
