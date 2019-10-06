#!/usr/bin/env gsi

(include "utils.scm")
(include "env.scm")
(include "types.scm")
(include "reader.scm")
(include "printer.scm")
(include "core.scm")

(define (READ str)
  (read-str str))

(define (macro-call? ast env)
  (and (pair? ast)
       (symbol? (car ast))
       (macro? (env-find env (car ast)))))

(define (MACROEXPAND ast env)
  (if (macro-call? ast env)
    (let* ((val (env-find env (car ast)))
           (fun (macro-fun val)))
      (cond ((closure? fun)
                 (MACROEXPAND (closure-apply fun (cdr ast) EVAL) env))
            ((procedure? fun)
                 (MACROEXPAND (apply fun (cdr ast)) env))
            (else
                 (error "MACROEXPAND - function expected"))))
    ast))

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

(define (EVAL ast* env)
  (define ast (MACROEXPAND ast* env))

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
               ((defmacro!)
                    (unless (= (length ast) 3)
                      (error "defmacro! - syntax error"))
                    (unless (symbol? (list-ref ast 1))
                      (error "defmacro! - symbol expected"))
                    (let ((val (EVAL (list-ref ast 2) env)))
                      (unless (or (procedure? val) (closure? val))
                        (error "defmacro! - function expected"))
                      (let ((mac (make-macro val)))
                        (env-set! env (list-ref ast 1) mac)
                        mac)))
               ((macroexpand)
                    (unless (= (length ast) 2)
                      (error "macroexpand - syntax error"))
                    (MACROEXPAND (list-ref ast 1) env))
               ((try*)
                    (let ((len (length ast)))
                      (unless (or (= len 2)
                                  (and (= len 3)
                                       (pair? (list-ref ast 2))
                                       (= (length (list-ref ast 2)) 3)
                                       (eq? (car (list-ref ast 2)) 'catch*)
                                       (symbol? (list-ref (list-ref ast 2) 1))))
                        (error "try*/catch* - syntax exception"))
                      (with-exception-catcher
                        (lambda (exc)
                          (cond ((mal.exc? exc)
                                     (if (= len 3)
                                       (let ((new-env (create-env env))
                                             (catch-exp (list-ref ast 2)))
                                         (env-set! new-env (list-ref catch-exp 1) (mal.exc-value exc))
                                         (EVAL (list-ref catch-exp 2) new-env))
                                       (mal.exc-value exc)))
                                ((error-exception? exc)
                                     (if (= len 3)
                                       (let ((new-env (create-env env))
                                             (catch-exp (list-ref ast 2)))
                                         (env-set! new-env (list-ref catch-exp 1) (error-exception-message exc))
                                         (EVAL (list-ref catch-exp 2) new-env))
                                       (error-exception-message exc)))
                                (else
                                     (raise exc))))
                        (lambda ()
                          (EVAL (list-ref ast 1) env)))))
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
                              (cond ((error-exception? exc)
                                         (display (sprint* "error: " (error-exception-message exc) "\n")))
                                    ((mal.exc? exc)
                                         (display (sprint* "exception: " (pr-str (mal.exc-value exc)) "\n")))
                                    (else
                                         (pp exc))))
                            (lambda ()
                              (rep str env)))))
                 (unless (or (eof-object? val) (void? val))
                   (display val)
                   (newline)))
               (repl env)))))

(define macros #<<end-of-macros
(do
(defmacro! fn (fn* [args body & rest]
  (if (empty? rest)
    `(fn* ~args ~body)
    `(fn* ~args (do ~body ~@rest)))))

(defmacro! let (fn* [vars body & rest]
  (if (empty? rest)
    `(let* ~vars ~body)
    `(let* ~vars (do ~body ~@rest)))))

(defmacro! defmacro (fn* [name args & body]
  `(defmacro! ~name (fn ~args ~@body))))

(defmacro def [& args]
  `(def! ~@args))

(defmacro defn [name args & body]
  `(def! ~name (fn ~args ~@body)))

(defn identity [x]
  x)

(def gensym
  (let [counter (atom 0)]
    (fn []
      (symbol (str "G__" (swap! counter + 1))))))

(defmacro time [exp]
  (let [start (gensym)
        ret   (gensym)]
    `(let [~start (time-ms)
           ~ret   ~exp]
       (println "Elapsed time:" (- (time-ms) ~start) "ms")
       ~ret)))

(defmacro cond [& xs]
  (let [cnt (count xs)]
    (if (= cnt 0) nil
      (if (< cnt 2)
        (throw "cond - even number of forms is required")
        `(if ~(first xs) ~(nth xs 1)
           (cond ~@(rest (rest xs))))))))

(defmacro when [tst body & rest]
  (if (empty? rest)
      `(if ~tst ~body)
      `(if ~tst (do ~body ~@rest))))

(defmacro unless [tst body & rest]
  (if (empty? rest)
    `(if ~tst nil ~body)
    `(if ~tst nil (do ~body ~@rest))))

(defmacro or [& xs]
  (if (empty? xs)
    false
    (let [sym (gensym)]
      `(let [~sym ~(first xs)]
         (if ~sym ~sym (or ~@(rest xs)))))))

(defmacro and [& xs]
  (if (empty? xs) true
    (if (empty? (rest xs))
      `(do ~@xs)
      `(if ~(first xs) (and ~@(rest xs)) false))))

(defn not [x]
  (if x false true))

(defn inc [n]
  (+ n 1))

(defn dec [n]
  (- n 1))

(defn zero? [x]
  (or (= x 0)
      (= x 0.0)))

(def! load-file
  (fn* [f]
    (eval (read-string (str "(do " (slurp f) "\nnil)")))))
)
end-of-macros
)

(define (main . argv)
  (let ((env (make-top-env)))
    (env-set! env 'eval (lambda (ast) (EVAL ast env)))
    (env-set! env '*host-language* "gambit")
    (env-set! env '*ARGV* (if (null? argv) argv (cdr argv)))
    (EVAL (read-str macros) env)
    (if (null? argv)
      (begin
        (EVAL '(println (str "Mal [" *host-language* "]")) env)
        (repl env))
      (EVAL `(load-file ,(car argv)) env))))
