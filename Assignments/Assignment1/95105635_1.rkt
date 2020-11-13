#lang racket
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))


(define (main inlist)
  (cond
    [(atom? inlist) inlist]
    [(null? inlist) `()]
    [(list? inlist) (append (main (cdr inlist)) (list (main (car inlist))))]
    )
  )

(display (main `(1 2 (3 4))))