#!/usr/bin/env gsi

(include "utils.scm")
(include "env.scm")
(include "types.scm")
(include "reader.scm")
(include "printer.scm")

(define (READ str)
  (read-str str))

(define (eval-ast ast env)
  (cond ((symbol? ast)
             (env-get env ast))
        ((pair? ast)
             (map (lambda (elm) (EVAL elm env)) ast))
        ((vector? ast)
             (vector-map (lambda (elm) (EVAL elm env)) ast))
        ((table? ast)
             (list->table
               (map (lambda (ap)
                      (cons (car ap) (EVAL (cdr ap) env)))
                    (table->list ast))))
        (else
             ast)))

(define (EVAL ast env)
  (let ((val (eval-ast ast env)))
    (cond ((pair? val)
               (apply (car val) (cdr val)))
          (else val))))

(define (PRINT form)
  (if (eof-object? form) form
      (pr-str form)))

(define (rep str env)
  (-> str
      READ
      (EVAL env)
      PRINT))

(define (repl env)
  (display "user> ")
  (let ((str (read-line)))
    (cond ((eof-object? str)  (newline))
          ((equal? str "")    (repl env))
          (else
               (let ((val (with-exception-catcher
                            (lambda (exc)
                              (if (error-exception? exc)
                                (display (sprint* "error: " (error-exception-message exc) "\n"))
                                (pp exc)))
                            (lambda ()
                              (rep str env)))))
                 (unless (or (eof-object? val) (void? val))
                   (display val)
                   (newline)))
               (repl env)))))

(define (make-top-env)
  (let ((env (create-env)))
    (env-set! env '+ +)
    (env-set! env '- -)
    (env-set! env '* *)
    (env-set! env '/ /)
    env))

(define (main . argv)
  (repl (make-top-env)))
