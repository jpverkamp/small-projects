#|
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer. 

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.  
    
    (3)The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
|#
(library (hash)
  (export make-hash hash-ref hash-set! hash-unset! hash->list list->hash)
  (import (chezscheme))

  (define-record :hash: (f nul del keys vals))

  ; given a hash function and a size for the table, create a hash
  (define (make-hash hash-function size)
    (let ([nul (gensym)])
      (make-:hash:
        (lambda (key)
          (mod (hash-function key) size))
        nul
        (gensym)
        (make-vector size nul)
        (make-vector size nul))))

  ; unpack a :hash:
  (define (*unpack h)
    (values
      (:hash:-f h) (:hash:-nul h) (:hash:-del h)
      (:hash:-keys h) (:hash:-vals h)))

  ; convert a hash into an index
  ; start at the hashed index and look for either that key or nul
  ; error (hash is full) if we loop
  ; set del? to return a del? index
  (define (*get-index h k del?)
    (let-values ([(f nul del ks vs) (*unpack h)])
      (let ([i0 (f k)])
        (let loop ([i i0] [fst #t])
          (cond
            [(and (not fst) (= i i0)) (error 'hash "hash is full")]
            [(equal? (vector-ref ks i) k) i]
            [(equal? (vector-ref ks i) nul) i]
            [(and del? (equal? (vector-ref ks i) del)) i]
            [else (loop (mod (+ i 1) 1000) #f)])))))

  ; get the value associated with a key k out of the hash h
  ; error if they key has not been set
  (define (hash-ref h k)
    (let-values ([(f nul del ks vs) (*unpack h)])
      (let ([i (*get-index h k #f)])
        (cond
          [(equal? (vector-ref ks i) nul)
           (error 'hash "key not set")]
          [else
           (vector-ref vs i)]))))

  ; get the value associated with key k out of the hash h
  (define (hash-set! h k v)
    (let-values ([(f nul del ks vs) (*unpack h)])
      (let ([i (*get-index h k #t)])
        (vector-set! ks i k)
        (vector-set! vs i v))))

  ; remove a given key from the hash
  (define (hash-unset! h k)
    (let-values ([(f nul del ks vs) (*unpack h)])
      (let ([i (*get-index h k #t)])
        (vector-set! ks i del)
        (vector-set! vs i del))))

  ; convert a hash into a list of key, value pairs
  (define (hash->list h)
    (let-values ([(f nul del ks vs) (*unpack h)])
      (let loop ([i 0])
        (cond
          [(= i (vector-length ks)) '()]
          [(or (equal? (vector-ref ks i) nul)
               (equal? (vector-ref ks i) del))
           (loop (+ i 1))]
          [else
           (cons
             (list (vector-ref ks i) (vector-ref vs i))
             (loop (+ i 1)))]))))

  ; convert a list into a hash
  (define (list->hash hash-function size ls)
    (let ([h (make-hash hash-function size)])
      (for-each (lambda (e) (hash-set! h (car e) (cadr e))) ls)
      h))

  ; custom writer for the hash
  (record-writer (type-descriptor :hash:)
    (lambda (r p wr)
      (display "#[hash " p)
			(wr (hash->list r) p)
			(display "]" p)))
	)

