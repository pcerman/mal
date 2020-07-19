(define-structure reader  port token line row col)

(define (create-reader str/port)
  (if (string? str/port)
    (make-reader (open-input-string "")
                 (void) str/port 0 0)
    (make-reader str/port (void) #f 0 0)))

(define (reader-pos rdr)
  (vector (reader-row rdr)
          (reader-col rdr)))

(define (white-space-char? ch)
  (or (<= (char->integer ch) 32)
      (eqv? ch #\,)))

(define (skip-white-space str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond ((>= i len)
                 len)
            ((white-space-char? (string-ref str i))
                 (loop (1+ i)))
            (else
                 i)))))

(define (skip-comment str i)
  (let ((len (string-length str)))
    (let loop ((i i) (cr #f))
      (if (>= i len) len
          (case (string-ref str i)
            ((#\newline)
                 (1+ i))
            ((#\return)
                 (if cr i
                   (loop (1+ i) #t)))
            (else
                 (if cr i
                   (loop (1+ i) #f))))))))

(define (skip-string str i)
  (let ((len (string-length str)))
    (let loop ((i (1+ i)) (esc #f))
      (cond ((>= i len)
                 len)
            (esc
                 (loop (1+ i) #f))
            (else
                 (case (string-ref str i)
                   ((#\\) (loop (1+ i) #t))
                   ((#\") (1+ i))
                   (else  (loop (1+ i) #f))))))))

(define (skip-atom str i)
  (let ((len (string-length str)))
    (let loop ((i i))
      (if (>= i len) len
        (let ((ch (string-ref str i)))
          (cond ((white-space-char? ch)
                     i)
                ((memv ch '(#\[ #\] #\{ #\} #\( #\) #\' #\" #\` #\, #\;))
                     i)
                (else
                     (loop (1+ i)))))))))

(define (read-token rdr)
  (let ((line (cond ((reader-line rdr) => identity)
                    (else (let ((line (read-line (reader-port rdr))))
                            (reader-line-set! rdr line)
                            (reader-row-set! rdr (1+ (reader-row rdr)))
                            (reader-col-set! rdr 0)
                            line)))))
    (if (eof-object? line) line
        (let* ((len (string-length line))
               (beg (skip-white-space line))
               (end (let loop ((i beg))
                      (if (>= i len) i
                          (let ((ch (string-ref line i)))
                            (case ch
                              ((#\~)  (if (and (< (1+ i) len)
                                               (eqv? (string-ref line (1+ i)) #\@))
                                          (+ i 2)
                                          (1+ i)))
                              ((#\[ #\] #\{ #\} #\( #\) #\' #\` #\~ #\^ #\@)
                                   (1+ i))
                              ((#\;)
                                   (skip-comment line i))
                              ((#\")
                                   (skip-string line i))
                              (else
                                   (skip-atom line i))))))))
          (cond ((eqv? beg end)
                     (reader-line-set! rdr #f)
                     (reader-col-set! rdr (+ (reader-col rdr) len))
                     (read-token rdr))
                (else
                     (let ((tn (substring line beg end))
                           (ln (substring line end len)))
                       (reader-line-set! rdr (and (not (equal? ln "")) ln))
                       (reader-col-set! rdr (+ (reader-col rdr) end))
                       tn)))))))

(define (next-token rdr)
  (let ((token (reader-token rdr)))
    (if (void? token)
      (read-token rdr)
      (begin
        (reader-token-set! rdr (void))
        token))))

(define (peek-token rdr)
  (let ((token (reader-token rdr)))
    (if (not (void? token)) token
      (let ((token (read-token rdr)))
        (reader-token-set! rdr token)
        token))))

(define (read-list rdr)
  (let loop ((token (peek-token rdr)) (lst '()))
    (cond ((or (eof-object? token)
               (member token '("]" "}")))
               (error (sprint* "unbalanced list at: " (reader-pos rdr))))
          ((equal? token ")")
               (next-token rdr)
               (reverse lst))
          (else
               (let ((form (read-form rdr)))
                 (loop (peek-token rdr) (cons form lst)))))))

(define (read-vector rdr)
  (let loop ((token (peek-token rdr)) (lst '()))
    (cond ((or (eof-object? token)
               (member token '(")" "}")))
               (error (sprint* "unbalanced vector at: " (reader-pos rdr))))
          ((equal? token "]")
               (next-token rdr)
               (list->vector (reverse lst)))
          (else
               (let ((form (read-form rdr)))
                 (loop (peek-token rdr) (cons form lst)))))))

(define (read-hash-map rdr)
  (let ((ht (make-table init: nil)))
    (let loop ((token (peek-token rdr)) (key (void)))
      (cond ((or (eof-object? token)
                 (member token '(")" "]")))
                 (error (sprint* "unbalanced hash-map at: " (reader-pos rdr))))
            ((equal? token "}")
                 (next-token rdr)
                 (unless (void? key)
                   (error (sprint* "hash-map - missing value at: " (reader-pos rdr))))
                 ht)
            ((void? key)
                 (loop (peek-token rdr) (read-form rdr)))
            (else
                 (let ((form (read-form rdr)))
                   (table-set! ht key form)
                   (loop (peek-token rdr) (void))))))))

(define (read-string token)
  (let* ((port (open-input-string token))
         (str (with-exception-catcher
                (lambda (exc) #f)
                (lambda () (read port)))))
    (unless str
      (error "unbalanced string/escape char"))
    str))

(define (read-atom token)
  (cond ((string->number token)
             => identity)
        ((eqv? (string-ref token 0) #\:)
             (-> (substring token 1 (string-length token))
                 string->keyword))
        (else
             (let ((atom (string->symbol token)))
               (cond ((equal? atom 'true)  #t)
                     ((equal? atom 'false) #f)
                     ((equal? atom 'nil) nil)
                     (else atom))))))

(define (read-form rdr)
  (let ((token (next-token rdr)))
    (cond ((eof-object? token) token)
          ((equal? token "'")  (list 'quote (read-form rdr)))
          ((equal? token "`")  (list 'quasiquote (read-form rdr)))
          ((equal? token "~")  (list 'unquote (read-form rdr)))
          ((equal? token "~@") (list 'splice-unquote (read-form rdr)))
          ((equal? token "@")  (list 'deref (read-form rdr)))
          ((equal? token "^")  (let ((meta (read-form rdr)))
                                 (list 'with-meta (read-form rdr) meta)))
          ((equal? token "(")  (read-list rdr))
          ((equal? token "[")  (read-vector rdr))
          ((equal? token "{")  (read-hash-map rdr))
          (else
               (case (string-ref token 0)
                 ((#\;) (read-form rdr))
                 ((#\") (read-string token))
                 (else (read-atom token)))))))

(define (read-str str)
  (let ((rdr (create-reader str)))
    (read-form rdr)))
