(define *meta-table* (make-table init: nil weak-keys: #t test: eq?))

(define (mal.meta arg)
  (table-ref *meta-table* arg))

(define (mal.with-meta arg meta)
  (cond ((or (member arg (list nil #t #f))
             (number? arg)
             (symbol? arg)
             (keyword? arg))
             (error "with-meta - unexpected type"))
        ((string? arg)
             (let ((str (string-append arg "")))
               (table-set! *meta-table* str meta)
               str))
        ((pair? arg)
             (let ((lst (list-copy arg)))
               (table-set! *meta-table* lst meta)
               lst))
        ((vector? arg)
             (let ((vec (vector-copy arg)))
               (table-set! *meta-table* vec meta)
               vec))
        ((closure? arg)
             (let ((cls (closure-clone arg)))
               (table-set! *meta-table* cls meta)
               cls))
        ((procedure? arg)
             (let ((cls (EVAL `(fn* (& args) (,mal.apply ,arg args)) (create-env))))
               (table-set! *meta-table* cls meta)
               cls))
        ((macro? arg)
             (let ((mac (macro-clone arg)))
               (table-set! *meta-table* mac meta)
               mac))
        (else
             (table-set! *meta-table* arg meta)
             arg)))

(define (mal.prn . args)
  (unless (null? args)
    (display (pr-str (car args) #t))
    (for-each (lambda (arg)
                (display #\space)
                (display (pr-str arg #t)))
              (cdr args)))
  (newline)
  nil)

(define (mal.println . args)
  (unless (null? args)
    (display (pr-str (car args) #f))
    (for-each (lambda (arg)
                (display #\space)
                (display (pr-str arg #f)))
              (cdr args)))
  (newline)
  nil)

(define (mal.list? arg)
  (or (pair? arg) (null? arg)))

(define (mal.= arg1 arg2)
  (cond ((and (vector? arg1) (vector? arg2))
             (let ((len1 (vector-length arg1)))
               (and (= len1 (vector-length arg2))
                    (let loop ((i 0))
                      (or (>= i len1)
                          (and (mal.= (vector-ref arg1 i)
                                      (vector-ref arg2 i))
                               (loop (1+ i))))))))
        ((and (pair? arg1) (pair? arg2))
             (let loop ((arg1 arg1) (arg2 arg2))
               (cond ((null? arg1)
                          (null? arg2))
                     ((null? arg2)
                          #f)
                     ((mal.= (car arg1) (car arg2))
                          (loop (cdr arg1) (cdr arg2)))
                     (else
                          #f))))
        ((and (vector? arg1) (mal.list? arg2))
             (mal.= (vector->list arg1) arg2))
        ((and (mal.list? arg1) (vector? arg2))
             (mal.= arg1 (vector->list arg2)))
        ((and (table? arg1) (table? arg2))
             (and (= (table-length arg1) (table-length arg2))
                  (let ((not-found (list 'not-found)))
                    (let loop ((al1 (table->list arg1)))
                      (if (null? al1) #t
                        (let ((va2 (table-ref arg2 (caar al1) not-found)))
                          (cond ((eq? va2 not-found)    #f)
                                ((mal.= (cdar al1) va2) (loop (cdr al1)))
                                (else #f))))))))
        (else
             (equal? arg1 arg2))))

(define (mal.empty? arg)
  (cond ((vector? arg)  (zero? (vector-length arg)))
        ((pair? arg)    #f)
        ((null? arg)    #t)
        ((nil? arg)     #t)
        (else           (error "empty? - sequence is expected"))))

(define (mal.count arg)
  (cond ((vector? arg)  (vector-length arg))
        ((pair? arg)    (length arg))
        ((null? arg)    0)
        ((nil? arg)     0)
        (else           (error "count - sequence is expected"))))

(define (mal.pr-str . args)
  (string-join (map (lambda (arg) (pr-str arg #t)) args) " "))

(define (mal.str . args)
  (string-join (map (lambda (arg) (pr-str arg #f)) args) ""))

(define (mal.slurp filename)
  (unless (file-exists? filename)
    (error "slurp - file not exists"))
  (fold-right string-append ""
    (call-with-input-file filename
      (lambda (port)
        (read-all port
          (lambda (p)
            (read-line p #\newline #t)))))))

(define (mal.reset! atm val)
  (atom-value-set! atm val)
  val)

(define (mal.swap! atm fn . args)
  (let ((val (cond ((closure? fn)
                        (closure-apply fn (cons (atom-value atm) args) EVAL))
                   ((procedure? fn)
                        (apply fn (cons (atom-value atm) args)))
                   (else
                        (error "swap! - function expexted")))))
    (atom-value-set! atm val)
    val))

(define (mal.cons elm arg)
  (cond ((nil? arg)     (list elm))
        ((null? arg)    (list elm))
        ((vector? arg)  (cons elm (vector->list arg)))
        (else           (cons elm arg))))

(define (mal.concat . args)
  (fold-right (lambda (arg val)
                (cond ((nil? arg)      val)
                      ((vector? arg)   (append (vector->list arg) val))
                      (else            (append arg val))))
              '() args))

(define (mal.first arg)
  (cond ((pair? arg)
             (car arg))
        ((or (null? arg) (nil? arg))
             nil)
        ((vector? arg)
             (if (> (vector-length arg) 0)
               (vector-ref arg 0)
               nil))
        (else
             (error "first - non empty sequence is expected"))))

(define (mal.rest arg)
  (cond ((pair? arg)
             (cdr arg))
        ((or (null? arg) (nil? arg))
             '())
        ((vector? arg)
             (if (> (vector-length arg) 1)
               (cdr (vector->list arg))
               '()))
        (else
             (error "rest - non empty sequence is expected"))))

(define (mal.nth arg idx)
  (cond ((pair? arg)
             (let loop ((lst arg) (idx idx))
               (cond ((or (null? lst) (< idx 0))
                          (error "nth - index is out of range"))
                     ((zero? idx)
                          (car lst))
                     (else
                          (loop (cdr lst) (1- idx))))))
        ((vector? arg)
             (cond ((or (< idx 0)
                        (>= idx (vector-length arg)))
                        (error "nth - index is out of range"))
                   (else
                        (vector-ref arg idx))))
        ((null? arg)
             (error "nth - index is out of range"))
        (else
             (error "nth - sequence is expected"))))

(define (non-atom-char?  ch)
  (or (memv ch '(#\space #\( #\) #\[ #\] #\{ #\} #\' #\" #\` #\, #\;))
      (< (char->integer ch) 32)))

(define (mal.symbol arg)
  (cond ((not (string? arg))
             (error "symbol - string is expected"))
        ((equal? arg "")
             (error "symbol - non empty string is expected"))
        ((string-any? non-atom-char? arg)
             (error "symbol - invalid character"))
        ((or (string->number arg)
             (member arg `("true" "false" "nil")))
             (error "symbol - invalid name"))
        (else
             (string->symbol arg))))

(define (mal.keyword arg)
  (cond ((keyword? arg) arg)
        ((not (string? arg))
             (error "keyword - string is expected"))
        ((equal? arg "")
             (string->keyword arg))
        ((string-any? non-atom-char? arg)
            (error "keyword - invalid character"))
        ((eqv? (string-ref arg 0) #\:)
             (-> (substring arg 1 (string-length arg))
                 string->keyword))
        (else
             (string->keyword arg))))

(define (mal.hash-map . args)
  (let ((ht (make-table init: nil)))
    (let loop ((args args) (key #f))
      (cond ((null? args)
                 (when key
                   (error "hash-map - odd number of arguments"))
                 ht)
            (key
                 (table-set! ht key (car args))
                 (loop (cdr args) #f))
            ((or (string? (car args))
                 (integer? (car args))
                 (keyword? (car args))
                 (symbol? (car args)))
                 (loop (cdr args) (car args)))
            (else
                 (error "hash-map - only string/integer/keyword/symbol is allowed for key"))))))

(define (mal.assoc arg . args)
  (unless (table? arg)
    (error "assoc - hash-map is expected"))
  (if (null? args) arg
    (table-merge! (apply mal.hash-map args) arg)))

(define (mal.dissoc arg . args)
  (unless (table? arg)
    (error "dissoc - hash-map is expected"))
  (let ((ht (table-copy arg)))
    (for-each (lambda (key) (table-set! ht key)) args)
    ht))

(define (mal.get hm key)
  (cond ((nil? hm)   nil)
        ((table? hm) (table-ref hm key nil))
        (else (error "get - hash-map is expected"))))

(define (mal.contains? hm key)
  (cond ((table? hm) (not (not-found? (table-ref hm key *not-found*))))
        (else (error "contains? - hash-map is expected"))))

(define (mal.keys hm)
  (cond ((table? hm) (map car (table->list hm)))
        (else (error "keys - hash-map is expected"))))

(define (mal.values hm)
  (cond ((table? hm) (map cdr (table->list hm)))
        (else (error "values - hash-map is expected"))))

(define (mal.apply fn . args)
  (define (%apply lst)
    (cond ((closure? fn)
               (closure-apply fn lst EVAL))
          ((procedure? fn)
               (if (null? lst) (fn)
                   (apply fn lst)))
          (else (error "apply - function is expected"))))

  (if (null? args)
    (%apply args)
    (let ((la (last args)))
      (cond ((pair? la)   (%apply (append (drop-right args 1) la)))
            ((vector? la) (%apply (append (drop-right args 1) (vector->list la))))
            ((null? la)   (%apply (drop-right args 1)))
            (else         (%apply args))))))

(define (mal.map fn seq)
  (define (%map lst)
    (cond ((closure? fn)
               (map (lambda (elm)
                      (closure-apply fn (list elm) EVAL))
                    lst))
          ((procedure? fn)
               (map fn lst))
          (else (error "map - function is expected"))))

  (cond ((pair? seq)
             (%map seq))
        ((vector? seq)
             (%map (vector->list seq)))
        ((or (null? seq) (nil? seq))
             '())
        (else
             (error "map - sequence is expected"))))

(define (mal.conj seq . args)
  (cond ((null? args)
             seq)
        ((pair? seq)
             (append-reverse args seq))
        ((or (null? seq)
             (nil? seq))
             (reverse args))
        ((vector? seq)
             (vector-append seq (list->vector args)))
        (else
             (error "conj - sequence is expected"))))

(define (mal.throw arg)
  (raise (make-mal.exc arg)))

(define (mal.readline arg)
  (display arg)
  (let ((line (read-line)))
    (if (eof-object? line)
      (begin (newline) nil)
      line)))

(define (mal.time-ms)
  (-> (current-time)
      time->seconds
      (lambda (x) (round (* 1000 x)))
      inexact->exact))

(define (mal.seq arg)
  (cond ((or (nil? arg) (equal? arg "") (null? arg))
             nil)
        ((pair? arg)
             arg)
        ((vector? arg)
             (if (zero? (vector-length arg)) nil
                 (vector->list arg)))
        ((string? arg)
             (map string (string->list arg)))
        (else
             (error "seq - sequence is expected"))))

(define (to-mal arg)
  (cond ((char? arg)   (string arg))
        ((pair? arg)   (map to-mal arg))
        ((vector? arg) (vector-map to-mal arg))
        (else          arg)))

(define (mal.scheme-eval str)
  (unless (string? str)
    (error "scheme-eval - string is expected"))
  (with-exception-catcher
    (lambda (exc)
      nil)
    (lambda ()
      (let ((port (open-input-string str)))
        (to-mal (eval (read port)))))))

(define (make-top-env)
  (let ((env (create-env)))
    (for-each (lambda (p)
                (env-set! env (car p) (cdr p)))
              `((+           . ,+)
                (-           . ,-)
                (*           . ,*)
                (/           . ,/)
                (quot        . ,quotient)
                (mod         . ,modulo)
                (rem         . ,remainder)
                (<           . ,<)
                (<=          . ,<=)
                (>           . ,>)
                (>=          . ,>=)
                (=           . ,mal.=)
                (list?       . ,mal.list?)
                (list        . ,list)
                (empty?      . ,mal.empty?)
                (count       . ,mal.count)
                (prn         . ,mal.prn)
                (println     . ,mal.println)
                (pr-str      . ,mal.pr-str)
                (str         . ,mal.str)
                (read-string . ,read-str)
                (slurp       . ,mal.slurp)
                (atom        . ,make-atom)
                (atom?       . ,atom?)
                (deref       . ,atom-value)
                (reset!      . ,mal.reset!)
                (swap!       . ,mal.swap!)
                (cons        . ,mal.cons)
                (concat      . ,mal.concat)
                (first       . ,mal.first)
                (rest        . ,mal.rest)
                (nth         . ,mal.nth)
                (nil?        . ,nil?)
                (true?       . ,(lambda (arg) (eq? arg #t)))
                (false?      . ,(lambda (arg) (eq? arg #f)))
                (symbol      . ,mal.symbol)
                (symbol?     . ,symbol?)
                (keyword     . ,mal.keyword)
                (keyword?    . ,keyword?)
                (vector      . ,vector)
                (vector?     . ,vector?)
                (sequential? . ,(lambda (arg)
                                  (or (mal.list? arg) (vector? arg))))
                (hash-map    . ,mal.hash-map)
                (map?        . ,table?)
                (assoc       . ,mal.assoc)
                (dissoc      . ,mal.dissoc)
                (get         . ,mal.get)
                (contains?   . ,mal.contains?)
                (keys        . ,mal.keys)
                (vals        . ,mal.values)
                (apply       . ,mal.apply)
                (map         . ,mal.map)
                (conj        . ,mal.conj)
                (throw       . ,mal.throw)
                (readline    . ,mal.readline)
                (time-ms     . ,mal.time-ms)
                (fn?         . ,(lambda (arg)
                                  (or (closure? arg) (procedure? arg))))
                (macro?      . ,macro?)
                (string?     . ,string?)
                (number?     . ,number?)
                (integer?    . ,integer?)
                (int?        . ,integer?)
                (rational?   . ,rational?)
                (real?       . ,(lambda (arg)
                                  (and (inexact? arg) (real? arg))))
                (complex?    . ,complex?)
                (exact?      . ,exact?)
                (inexact?    . ,inexact?)
                (int         . ,truncate)
                (real        . ,exact->inexact)
                (seq         . ,mal.seq)
                (meta        . ,mal.meta)
                (with-meta   . ,mal.with-meta)
                (scheme-eval . ,mal.scheme-eval)
                ))
    env))
