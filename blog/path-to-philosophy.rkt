#lang racket

(require
 json
 net/url
 racket/path
 racket/runtime-path
 racket/string)

(define wikipedia-api-url "http://en.wikipedia.org/w/api.php?format=json&action=query&titles=~a&prop=revisions&rvprop=content")

(define DEBUG_MODE #f)
(define (debug-printf fmt . args)
  (when DEBUG_MODE
    (printf "[DEBUG] ")
    (apply printf (cons fmt args))))

; Make sure the cache exists
(define-runtime-path cache "cache")
(unless (directory-exists? cache)
  (make-directory cache))

; Fetch pages from Wikipedia, caching them to disk
(define (get-page title [skip-cache #f])
  ; If the file isn't cached, fetch it
  (define clean-title (regexp-replace* #rx"[^a-zA-Z0-9._-]+" title "-"))
  (define path (build-path cache (format "~a.json" clean-title)))
  (when (or skip-cache (not (file-exists? path)))
    (define url (string->url (format wikipedia-api-url title)))
    (define http-in (get-pure-port url #:redirections 3))
    (define response (port->string http-in))
    (close-input-port http-in)
    (with-output-to-file path
      (lambda ()
        (display response))))
  
  ; Load the file from cache (assume the previous worked)
  (string->jsexpr (file->string path)))

; Extract the first page content from a JSON encoded Wikipedia API response
(define (get-first-page-content page)
  (define page-keys (hash-keys (hash-ref (hash-ref page 'query) 'pages)))
  (define content (hash-ref (hash-ref (hash-ref page 'query) 'pages) (car page-keys)))
  (cond
    [(hash-has-key? content 'missing)
     (debug-printf "[DEBUG] missing content detected, skipping\n")
     ""]
    [else
     (hash-ref (car (hash-ref content 'revisions)) '*)]))

; Filter out any links in the page that are in paranthesis, italics, or tables
; Return a list of all remaining internal links
(define (filter-links page-content)
  (define upper-bound (- (string-length page-content) 1))
  
  ; Does the string at a given position start with a given text
  (define (text-at? pos str)
    (and (<= 0 pos upper-bound)
         (<= 0 (+ pos (string-length str)) (+ upper-bound 1))
         (equal? str (substring page-content pos (+ pos (string-length str))))))
  
  ; Return the position right after a closing bracket
  ; Ignore nested pairs
  (define (find-matching open close start)
    (let loop ([position start] [nested 0])
      (define at-opening (and open (text-at? position open)))
      (define at-closing (and close (text-at? position close)))
      (cond
        [(> position upper-bound)
         position]
        [(and at-closing (= nested 0))
         (+ position 2)]
        [at-closing
         (loop (+ position 2) (- nested 1))]
        [at-opening
         (loop (+ position 1) (+ nested 1))]
        [else
         (loop (+ position 1) nested)])))
  
  ; Pull out any links, ignoring certain patterns
  (define links
    (let loop ([position 0])
      (cond
        ; Done looking
        [(>= position (string-length page-content))
         '()]
        ; Found subcontent {{content}}, ignore it
        [(text-at? position "{{")
         (define end (find-matching "{{" "}}" (+ position 2)))
         (loop end)]
        ; Found bracketed italics content ''content'', ignore it
        ; Note: open #f because these can't next
        [(text-at? position "''")
         (define end (find-matching #f "''" (+ position 2)))
         (loop end)]
        ; Found paranthesis, ignore them
        [(text-at? position "(")
         (define end (find-matching "(" ")" (+ position 1)))
         (loop end)]
        ; Found a File/image block [[File/Image: content]], ignore it
        ; Note: may contain nested [[links]] we don't want
        [(or (text-at? position "[[File:")
             (text-at? position "[[Image:"))
         (define end (find-matching "[[" "]]" (+ position 2)))
         (loop end)]
        ; Found a link: [[content]], return it
        [(text-at? position "[[")
         (define end (find-matching "[[" "]]" (+ position 2)))
         (cons (substring page-content position end)
               (loop end))]
        ; Otherwise, just cane forward
        [else
         (loop (+ position 1))])))
  
  ; Get just the targets, remove any external links
  (define (split-link link) (string-split link "|"))
  (define (remove-brackets link) (substring link 2 (- (string-length link) 2)))
  (define (not-external? link) (or (< (string-length link) 4) (not (equal? "http" (substring link 0 4)))))
  (map string-downcase (filter not-external? (map car (map split-link (map remove-brackets links))))))

; Helper function to run a nested regex over until it's all gone
(define (regexp-repeated-remove pattern input)
  (if (regexp-match pattern input)
      (regexp-repeated-remove pattern (regexp-replace pattern input ""))
      input))

; Get the neighbors to a given article
(define (get-neighbors title)
  (filter-links (get-first-page-content (get-page title))))

; Find a path from one page to another, depth first search
(define (find-path from to)
  (set! from (string-downcase from))
  (set! to (string-downcase to))
  (let loop ([page from] [sofar '()])
    (debug-printf "[DEBUG] Current page: ~a\n" page)
    (cond
      [(equal? page to) (list to)]
      [(member page sofar) 
       (debug-printf "[DEBUG] loop detected at ~a, backtracking\n" page)
       #f]
      [else
       ; Depth first search, try all neighbors
       (let neighbor-loop ([neighbors (get-neighbors page)])
         (cond
           ; Out of neighbors, this page is useless
           [(null? neighbors)
            (debug-printf "[DEBUG] no paths from ~a, backtracking\n" page)
            #f]
           ; Found a recursive match, build backwards
           [(loop (car neighbors) (cons page sofar))
            => (lambda (next) (cons page next))]
           ; Didn't find a match, try the next neighbor
           [else
            (debug-printf "[DEBUG] trying next neighbor for ~a: ~a\n" page (cdr neighbors))
            (neighbor-loop (cdr neighbors))]))])))

; Starting at a given page and taking the first link, how long to Philosophy?
(define (->philosophy start) (find-path start "philosophy"))
