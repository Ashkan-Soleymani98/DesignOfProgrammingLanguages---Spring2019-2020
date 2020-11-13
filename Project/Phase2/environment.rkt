#lang racket
(require (except-in eopl #%module-begin))

;Thunk definition
(define thunk
  (lambda (exp env process-env)
    (list 'thunk exp env process-env)
    )
  )

(define (is-thunk? a-thunk)
  (and (list? a-thunk) (not (null? a-thunk)) (eqv? (car a-thunk) 'thunk))
  )
        
;Procedure Definition
(define (has-duplicate? lst) 
  (cond
     [(empty? lst) #f] 
     [(not (not (member (first lst) (rest lst)))) #t]
     [else (has-duplicate? (rest lst))])
  )

(define procedure
  (lambda (name vars body saved-env)
    (if (has-duplicate? vars)
        (report-function-duplicate-formal-parameters name)
        (list 'procedure name vars body saved-env))
    )
  )

(define lib-procedure
  (lambda (name vars-length)
    (list 'lib-procedure name vars-length)
    )
  )

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

(define extend-env-multi
  (lambda (pname vars vals env)
    (if (equal? (length vars) (length vals))
        (if (null? vars)
            env
            (append (list 'extend-env (car vars) (car vals)) (list (extend-env-multi pname (cdr vars) (cdr vals) env))))
        (report-function-arguments-size-missmatch pname))
    )
  )

(define extend-env-rec
  (lambda (p-name b-var p-body p-env env)
    (list 'extend-env-rec p-name b-var p-body p-env env)
    )
  )

(define extend-env-lib
  (lambda (p-name vars-length env)
    (list 'extend-env-lib p-name vars-length env)
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
      [(eqv? (car env) 'extend-env-rec)
       (let* ((p-name (cadr env))
             (b-vars (caddr env))
             (p-body (cadddr env))
             (temp (cddddr env))
             (p-env (car temp))
             (saved-env (cadr temp)))
         (if (equal? search-var p-name)
             (procedure p-name b-vars p-body p-env)
             (apply-env saved-env search-var)))]
      [(eqv? (car env) 'extend-env-lib)
       (let* ((p-name (cadr env))
             (vars-length (caddr env))
             (saved-env (cadddr env)))
         (if (equal? search-var p-name)
             (lib-procedure p-name vars-length)
             (apply-env saved-env search-var)))]
      [else (report-invalid-env env)])
    )
  )

(define report-is-not-callable
  (lambda (pname)
    (eopl:error 'extend-env-multi "Variable ~s is Not Callable" pname)
    )
  )

(define report-function-duplicate-formal-parameters
  (lambda (pname)
    (eopl:error 'extend-env-multi "Duplicate Formal Parameters for Function ~s" pname)
    )
  )

(define report-function-arguments-size-missmatch
  (lambda (pname)
    (eopl:error 'extend-env-multi "Function Arguments Missmatch for Function ~s" pname)
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

;(apply-env (extend-env "a" 3 (extend-env-rec "f" '("b") "(unitcommand (returncom (return (aexp (bexp (cexp (var a)))))))" (extend-env "a" 2 (empty-env)) (extend-env "a" 2 (empty-env)))) "f")
       