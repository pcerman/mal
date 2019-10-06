(define *outer* (box '*outer*))
(define *not-found* (box '*not-found*))

(define-macro (not-found? arg)
  `(eq? ,arg *not-found*))

(define (create-env #!optional (outer #f))
  (let ((env (make-table init: *not-found*)))
    (when outer
      (table-set! env *outer* outer))
    env))

(define (env-set! env key val)
  (table-set! env key val))

(define (env-find env key #!optional (default nil))
  (let ((val (table-ref env key)))
    (if (not-found? val)
      (let ((outer (table-ref env *outer*)))
        (if (not-found? outer)
          default
          (env-find outer key default)))
      val)))

(define (env-get env key)
  (let ((val (env-find env key *not-found*)))
    (if (not-found? val)
      (error (sprint* "'" (swrite* key) "' not found"))
      val)))
