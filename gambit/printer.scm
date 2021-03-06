
(define (pr-str arg #!optional (readably #t))
  (call-with-output-string
    (lambda (port)
      (letrec ((prn-list (lambda (lst)
                           (let loop ((lst lst))
                             (unless (null? lst)
                               (prn (car lst))
                               (unless (null? (cdr lst))
                                 (display " " port))
                               (loop (cdr lst))))))
               (prn (lambda (arg)
                      (cond ((or (pair? arg) (null? arg))
                                 (display "(" port)
                                 (prn-list arg)
                                 (display ")" port))
                            ((vector? arg)
                                 (display "[" port)
                                 (prn-list (vector->list arg))
                                 (display "]" port))
                            ((table? arg)
                                 (display "{" port)
                                 (prn-list (fold (lambda (kv va)
                                                   (cons* (car kv) (cdr kv) va))
                                                 '()
                                                 (table->list arg)))
                                 (display "}" port))
                            ((keyword? arg)
                                 (display #\: port)
                                 (display (keyword->string arg) port))
                            ((string? arg)
                                 (if readably
                                   (write arg port)
                                   (display arg port)))
                            ((closure? arg)
                                 ;; this is optional for debug. it can be removed
                                 (display "<closure [" port)
                                 (prn-list (closure-args arg))
                                 (display "] " port)
                                 (prn (closure-body arg))
                                 (display ">" port))
                            ((macro? arg)
                                 (let ((fun (macro-fun arg)))
                                   (cond ((closure? fun)
                                              (display "<macro [" port)
                                              (prn-list (closure-args fun))
                                              (display "] " port)
                                              (prn (closure-body fun))
                                              (display ">" port))
                                         (else
                                              (display "<macro " port)
                                              (display fun port)
                                              (display ">" port)))))
                            ((atom? arg)
                                 (display "(atom " port)
                                 (prn (atom-value arg))
                                 (display ")" port))
                            ((nil? arg)
                                 (display "nil" port))
                            ((eq? arg #f)
                                 (display 'false port))
                            ((eq? arg #t)
                                 (display 'true port))
                            (else
                                 (display arg port))))))
        (prn arg)))))
