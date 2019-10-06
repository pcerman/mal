#!/usr/bin/env gsi

(include "utils.scm")

(define READ identity)
(define EVAL identity)
(define PRINT identity)

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
          (else               (display (rep str))
                              (newline)
                              (repl)))))

(define (main . argv)
  (repl))
