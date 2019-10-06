#!/usr/bin/env gsi

(include "utils.scm")
(include "env.scm")
(include "types.scm")
(include "reader.scm")
(include "printer.scm")
(include "core.scm")

(define (READ str)
  (read-str str))

(define (QQUOTE ast env)
  (cond ((pair? ast)
             (case (car ast)
               ((unquote)        (EVAL (list-ref ast 1) env))
               ((splice-unquote) (EVAL (list-ref ast 1) env))
               (else
                    (fold-right (lambda (elm val)
                                  (let ((vv (QQUOTE elm env)))
                                    (if (and (pair? elm) (eq? (car elm) 'splice-unquote))
                                      (mal.concat vv val)
                                      (cons vv val))))
                                '()
                                ast))))
        ((vector? ast)
             (QQUOTE (vector->list ast) env))
        (else
             ast)))

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
                      (let ((key (EVAL (car ap) env)))
                        (unless (or (string? key)
                                    (integer? key)
                                    (keyword? key)
                                    (symbol? key))
                          (error "hash-map - only string/integer/keyword/symbol is allowed for key"))
                      (cons key (EVAL (cdr ap) env))))
                    (table->list ast))
               init: nil))
        (else
             ast)))

(define (EVAL ast env)
  (cond ((pair? ast)
             (case (car ast)
               ((def!)
                    (unless (= (length ast) 3)
                      (error "def! - syntax error"))
                    (unless (symbol? (list-ref ast 1))
                      (error "def! - symbol expected"))
                    (let ((val (EVAL (list-ref ast 2) env)))
                      (env-set! env (list-ref ast 1) val)
                      val))
               ((let*)
                    (unless (= (length ast) 3)
                      (error "let* - syntax error"))
                    (let ((new-env (create-env env))
                          (vars (list-ref ast 1)))
                      (let loop ((vars (seq->list vars)))
                        (cond ((null? vars)
                                   (EVAL (list-ref ast 2) new-env))
                              ((null? (cdr vars))
                                   (error "let* - missing value for binding"))
                              ((not (symbol? (car vars)))
                                   (error "let* - symbol expected for binding"))
                              (else
                                   (env-set! new-env (car vars) (EVAL (list-ref vars 1) new-env))
                                   (loop (cddr vars)))))))
               ((if)
                    (unless (<= 3 (length ast) 4)
                      (error "if - syntax error"))
                    (let ((tst (EVAL (list-ref ast 1) env)))
                      (if (or (not tst) (nil? tst))
                        (if (= (length ast) 3) nil
                          (EVAL (list-ref ast 3) env))
                        (EVAL (list-ref ast 2) env))))
               ((do)
                    (if (null? (cdr ast)) nil
                      (let loop ((ast (cdr ast)) (val nil))
                        (if (null? (cdr ast))
                          (EVAL (car ast) env)
                          (loop (cdr ast) (EVAL (car ast) env))))))
               ((fn*)
                    (unless (= (length ast) 3)
                      (error "fn* - syntax error"))
                    (create-closure env
                                    (seq->list (list-ref ast 1))
                                    (list-ref ast 2)))
               ((quote)
                    (unless (= (length ast) 2)
                      (error "quote - syntax error"))
                    (list-ref ast 1))
               ((quasiquote)
                    (unless (= (length ast) 2)
                      (error "quasiquote - syntax error"))
                    (QQUOTE (list-ref ast 1) env))
               (else
                    (let ((val (eval-ast ast env)))
                      (cond ((closure? (car val))
                                 (EVAL (closure-body (car val))
                                       (create-closure-env (car val) (cdr val))))
                            ((procedure? (car val))
                                 (apply (car val) (cdr val)))
                            (else
                                 (error "apply - function expected")))))))
        (else
             (eval-ast ast env))))

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

(define macros #<<end-of-macros
(do

(def! not
  (fn* [x]
    (if x false true)))

(def! load-file
  (fn* [f]
    (eval (read-string (str "(do " (slurp f) "\nnil)")))))

)
end-of-macros
)

(define (main . argv)
  (let ((env (make-top-env)))
    (env-set! env 'eval (lambda (ast) (EVAL ast env)))
    (env-set! env '*ARGV* (if (null? argv) argv (cdr argv)))
    (EVAL (read-str macros) env)
    (if (null? argv)
      (repl env)
      (EVAL `(load-file ,(car argv)) env))))
