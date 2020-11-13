#lang racket
(require test-engine/racket-tests)
(define (certain_max inlist)
  (cond
    [(null? inlist) 0]
    [else (max (car inlist) (+ (car inlist) (certain_max (cdr inlist)) ) )]
    )
  )
    
(define (uncertain_max inlist)
  (cond
    [(null? inlist) 0]
    [else (max (certain_max inlist) (uncertain_max (cdr inlist)))]
    )
  )
    
(define (main inlist)
  (uncertain_max inlist)
  )

(check-expect (main `(4 -5 8 2 -3 13 -6)) 20)