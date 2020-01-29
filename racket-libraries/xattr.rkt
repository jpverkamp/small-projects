#lang racket

(provide xattr-list
         xattr-read
         xattr-write
         xattr-delete
         xattr-clear
         xattr-read-all)

(define xattr
  (let ([exe (find-executable-path "xattr")])
    (λ args
      (string-trim
       (with-output-to-string
         (thunk
          (apply system* (cons exe args))))
       "\n"
       #:left? #f
       #:right? #t
       #:repeat? #f))))

(define (xattr-list file) (string-split (xattr file) "\n"))
(define (xattr-read file key) (xattr "-p" key file))
(define (xattr-write file key value) (xattr "-w" key value file))
(define (xattr-delete file key) (xattr "-d" key file))
(define (xattr-clear file) (xattr "-c" file))

(define (xattr-read-all file)
  (for/hash ([key (in-list (xattr-list file))])
    (values key (xattr-read file key))))


