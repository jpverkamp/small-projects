#lang racket

(require racket/generator)

(define (bytes->hex b*)
  (apply ~a (for/list ([b (in-bytes b*)])
              (~a (number->string (quotient b 16) 16)
                  (number->string (modulo b 16) 16)))))

(define (rc4 key msg)
  (when (string? key) (set! key (string->bytes/utf-8 key)))
  (when (string? msg) (set! msg (string->bytes/utf-8 msg)))
  
  (define (mod256 n) (modulo n 256))

  (define permutation (make-bytes 256))
  (for ([i (in-range 256)])
    (bytes-set! permutation i i))
  
  (define (S i)
    (bytes-ref permutation i))
  
  (define (swap! i j)
    (let ([pi (bytes-ref permutation i)]
          [pj (bytes-ref permutation j)])
      (bytes-set! permutation i pj)
      (bytes-set! permutation j pi)))

  ; Key-scheduling algorithm
  (for/fold ([j 0]) ([i (in-range 256)])
    (let ([j (mod256 (+ j
                        (S i)
                        (bytes-ref key (modulo i (bytes-length key)))))])
      (swap! i j)
      j))
  
  ; Pseudo-random generation algorithm
  (define prga
    (generator ()
      (let loop ([i 1] [j (S 1)])
        (swap! i j)
        (yield (S (mod256 (+ (S i) (S j)))))
        (loop (mod256 (+ i 1)) (mod256 (+ j (S (+ i 1))))))))
  
  ; Encryption
  (apply bytes 
         (for/list ([input-byte (in-bytes msg)] [key-byte (in-producer prga)])
           (bitwise-xor input-byte key-byte))))

  
