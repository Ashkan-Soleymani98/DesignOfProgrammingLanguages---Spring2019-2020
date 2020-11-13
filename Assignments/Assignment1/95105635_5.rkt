#lang racket
(define (dot-product row column)
  (cond
    [(zero? (length row)) 0]
    [else (+ (* (car row) (car column)) (dot-product (cdr row) (cdr column)))]
    )
  )
         
(define (rowmul-to-matrix row matrix)
  (cond
    [(zero? (length matrix)) `()]
    [else (cons (dot-product row (car matrix)) (rowmul-to-matrix row (cdr matrix)))]
    )
  )

(define (transpose matrix)
  (apply map list matrix))

(define (matmul matrix1 matrix2)
  (cond
    [(zero? (length matrix1)) `()]
    [else (cons (rowmul-to-matrix (car matrix1) (transpose matrix2)) (matmul (cdr matrix1) matrix2))]
    )
  )

(define (main matrix1 matrix2)
  (matmul matrix1 matrix2)
  )


;(display (dot-product `(1 2 12) `(-1 1 12)))
;(display (rowmul-to-matrix `(1 2 12) `((1 2) (1 1) (-1 1))))
(display (main `((2 1 0)(4 3 1)) `((-1 1)(1 1)(0 1)))) 