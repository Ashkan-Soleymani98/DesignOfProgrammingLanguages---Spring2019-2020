#lang racket
(require (except-in eopl #%module-begin))

;Enviornment Definition
(define empty-env
  (lambda ()
    (list 'empty-env)
    )
  )

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)
    )
  )

(define apply-env
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env) (report-no-binding-found search-var)]
      [(eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (equal? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)))]
      [else (report-invalid-env env)])
    )
  )

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No Binding for ~s, Variable Used Before Assignment" search-var)
    )
  )

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad Envoriment: ~s" env)
    )
  )
      
       
(define e (extend-env "d" 6
           (extend-env 'y 8
            (extend-env 'x 7
             (extend-env 'y 14
              (empty-env))))))


(provide (all-defined-out))
       