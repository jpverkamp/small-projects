#lang racket

; parse a block into a scheme object
; if that fails, just return the original string
(define (try-read block)
  (with-handlers ([exn? (lambda (err) block)])
    (with-input-from-string 
     block
     (lambda ()
       (define result (read))
       (when (not (eof-object? (read)))
         (error 'try-read "unfinished"))
       result))))

; read csv content from the current input stream
; try to parse each value as a Scheme value, on fail return a string
; make sure to correctly handle quoted strings
(define (read-csv)
  (let loop ([lines '()]
             [current-line '()]
             [current-block '()]
             [mode 'start]) ; start, string, string/escape, or normal
    
    (define next (read-char))
    (cond
      ; eof, stop reading
      [(eof-object? next)
       (reverse (cons (reverse (cons (try-read (list->string (reverse current-block)))
                                     current-line))
                      lines))]
      
      ; start in string or normal mode
      [(eq? mode 'start)
       (loop lines current-line (cons next current-block) (if (eq? next #\") 'string 'normal))]
      
      ; look for escaped characters in string mode
      [(and (eq? mode 'string) (eq? next #\\))
       (loop lines current-line (cons next current-block) 'string/escape)]
      
      ; read next character in escape mode
      [(eq? mode 'string/escape)
       (loop lines current-line (cons next current-block) 'string)]
      
      ; end the string in string mode (the next character must be a comma or newline)
      ; todo: deal with \r\n newlines
      [(and (eq? mode 'string) (eq? next #\"))
       (define done-block (try-read (list->string (reverse (cons next current-block)))))
       (define next-next (read-char))
       (cond 
         [(eq? next-next #\,)
          (loop lines (cons done-block current-line) '() 'start)]
         [(eq? next-next #\newline)
          (define done-line (reverse (cons done-block current-line)))
          (loop (cons done-line lines) '() '() 'start)]
         [else
          (error 'read-csv "Invalid string literal, missing comma or newline")])]
      
      ; end an item
      [(and (eq? mode 'normal) (eq? next #\,))
       (define done-block (try-read (list->string (reverse current-block))))
       (loop lines (cons done-block current-line) '() 'start)]
      
      ; end a line
      ; todo: deal with \r\n
      [(and (eq? mode 'normal) (eq? next #\newline))
       (define done-block (try-read (list->string (reverse current-block))))
       (define done-line (reverse (cons done-block current-line)))
       (loop (cons done-line lines) '() '() 'start)]
      
      ; buffer all other characters
      [else
       (loop lines current-line (cons next current-block) mode)])))

; write a list of lists to an html table
(define (csv->html csv)
  (with-output-to-string
   (lambda ()
     (printf "<table>\n")
     (for ([line (in-list csv)])
       (printf "  <tr>\n")
       (for ([item (in-list line)])
         (printf "    <td>~a</td>\n" item))
       (printf "  </tr>\n"))
     (printf "</table>\n"))))
    
; test case
(require rackunit)
(begin
  (define input "this,is a,\"test of awesome\"\n1,2,3.14\n#<void>,\"frog, \\\"neblins\\\"\",#f")
  (define parsed (with-input-from-string input read-csv))
  (define html (csv->html parsed))
  
  (check-equal? parsed '((this "is a" "test of awesome") (1 2 3.14) ("#<void>" "frog, \"neblins\"" #f)))
  (check-equal? html "<table>
  <tr>
    <td>this</td>
    <td>is a</td>
    <td>test of awesome</td>
  </tr>
  <tr>
    <td>1</td>
    <td>2</td>
    <td>3.14</td>
  </tr>
  <tr>
    <td>#<void></td>
    <td>frog, \"neblins\"</td>
    <td>#f</td>
  </tr>
</table>
"))
    