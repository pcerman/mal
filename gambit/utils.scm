(define-macro (-> va ex #!rest exs)
  (if (null? exs)
    (if (and (pair? ex) (not (eq? (car ex) 'lambda)))
      (cons* (car ex) va (cdr ex))
      (list ex va))
    `(-> (-> ,va ,ex) ,(car exs) ,@(cdr exs))))

(define-macro (->> va ex #!rest exs)
  (if (null? exs)
    (if (and (pair? ex) (not (eq? (car ex) 'lambda)))
      (append ex (list va))
      (list ex va))
    `(->> (->> ,va ,ex) ,(car exs) ,@(cdr exs))))

(define (sprint* #!rest args)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (arg)
                  (display arg port))
                args))))

(define (swrite* #!rest args)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (arg)
                  (write arg port))
                args))))

(define (void? arg)
  (eq? arg (void)))

(define (1+ n)
  (+ n 1))

(define (1- n)
  (- n 1))

(define (seq->list arg)
  (if (vector? arg)
    (vector->list arg)
    arg))

(define (append-reverse lst1 lst2)
  (if (null? lst1) lst2
    (append-reverse (cdr lst1) (cons (car lst1) lst2))))

(define (drop-right lst n)
  (let ((len (length lst)))
    (cond ((>= n len) '())
          (else (take lst (- len n))))))

(define (string-join strs join)
  (cond ((null? strs)
             "")
        ((null? (cdr strs))
             (car strs))
        (else
             (call-with-output-string
               (lambda (port)
                 (display (car strs) port)
                 (for-each (lambda (str)
                             (display join port)
                             (display str port))
                           (cdr strs)))))))
