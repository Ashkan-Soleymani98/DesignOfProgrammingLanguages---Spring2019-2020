#lang racket
(require (except-in eopl #%module-begin))

(define simple-list-ref2
  (lambda (lst place)
    (if (or (not (list? lst)) (<= (length lst) place))
        (report-index-out-of-bound lst place)
        (if (= place 0)
            (car lst)
            (simple-list-ref (cdr lst) (- place 1)))
        )
    )
  )


(define simple-list-ref
  (lambda (lst place)
    (cond
      [(not (list? lst)) (report-not-a-list lst place)]
      [(<= (length lst) place) (report-index-out-of-bound lst place)]
      [else (if (= place 0)
            (car lst)
            (simple-list-ref (cdr lst) (- place 1)))]
      )
    )
  )

(define report-index-out-of-bound
  (lambda (lst index)
    (eopl:error 'apply-env "List ~s Out of Bounds for Index ~s" lst index)
    )
  )

(define report-not-a-list
  (lambda (lst index)
    (eopl:error 'apply-env "~s Is Not a List" lst)
    )
  )

(define get-list-item
  (lambda (input-list indices)
    (if (zero? (length indices))
        input-list
        (get-list-item (simple-list-ref input-list (car indices)) (cdr indices))
        
      )
    )
  )


;(get-list-item (list (list (list 1 21 -2) (list 1 2) 12) -5) (list 0 1 2))

(provide (all-defined-out))