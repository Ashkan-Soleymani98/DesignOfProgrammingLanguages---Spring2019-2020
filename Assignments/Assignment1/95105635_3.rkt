#lang racket
(define (polynomial poly x)
  (cond
    [(= 1 (length poly)) (car poly)]
    [else (polynomial (cons (+ (* (car poly) x) (cadr poly)) (cddr poly)) x)]
    )
  )

;(display (polynomial `(1 2 3) 0.5)) (display "\n")

(define (calculate-area poly lower-bound upper-bound)
  (* (polynomial (reverse poly) (/ (+ lower-bound upper-bound) 2.0)) (- upper-bound lower-bound))
  )

(define (main input-list)
  (define lower-bound (cadr input-list))
  (define upper-bound (caddr input-list))
  (define counter (cadddr input-list))
  (cond
    [(zero? counter) 0]
    [else
     (define next-lower-bound (+ lower-bound (/ (- upper-bound lower-bound) counter)))
     (define next-configuration (cons next-lower-bound (cons upper-bound (cons (sub1 counter) `()))))
     (+ (+ (calculate-area (car input-list) lower-bound next-lower-bound)) (main (cons (car input-list) next-configuration)))]
    )
  )

(display (main `((1 2 3) 1 2 4)))