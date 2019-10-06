#!/usr/bin/env gsi

(include "utils.scm")
(include "env.scm")
(include "types.scm")
(include "reader.scm")
(include "printer.scm")

(define (READ str)
  (read-str str))

(define EVAL identity)

(define (PRINT form)
  (if (eof-object? form) form
      (pr-str form)))

(define (rep str)
  (-> str
      READ
      EVAL
      PRINT))

(define (repl)
  (display "user> ")
  (let ((str (read-line)))
    (cond ((eof-object? str)  (newline))
          ((equal? str "")    (repl))
          (else
               (let ((val (with-exception-catcher
                            (lambda (exc)
                              (if (error-exception? exc)
                                (display (sprint* "error: " (error-exception-message exc) "\n"))
                                (pp exc)))
                            (lambda ()
                              (rep str)))))
                 (unless (or (eof-object? val) (void? val))
                   (display val)
                   (newline)))
               (repl)))))

(define (main . argv)
  (repl))
