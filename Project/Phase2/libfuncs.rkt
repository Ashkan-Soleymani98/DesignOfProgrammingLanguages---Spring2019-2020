#lang racket

(require (except-in eopl #%module-begin))

(define (pow a b)
  (cond
    [(= a 0) 0]
    [(= b 0) 1]
    [else (* a (pow a (- b 1)))]
    ))

(define (makelist a b)
  (cond
    [(< a 1) '()]
    [else (append (list b) (makelist (- a 1) b))]
    ))

(define (reverse L)
  (cond
    [(null? L) '()]
    [else (append (reverse (rest L)) (list (first L)))]
    ))

(define (reverseall L)
  (cond
    [(null? L) '()]
    [(list? (first L)) (append (reverseall (rest L)) (list(reverseall (first L))))]
    [else (append (reverseall (rest L)) (list (first L)))]
    ))

(define (length L)
  (cond 
    [(null? L) 0]
    [else (+ 1 (length (rest L)))]
  ))

(define (set L ind val)
  (cond
    [(< (length L) (+ ind 1)) (eopl:error "Index is Out of Bound")]
    [(= ind 0) (append (list val) (rest L))]
    [else (append (list(first L)) (set (rest L) (- ind 1) val))]
    ))

(define (merge L1 L2)
  (cond
    [(null? L1) L2]
    [(null? L2) L1]
    [(< (first L1) (first L2)) (append (list(first L1)) (merge (rest L1) L2))]
    [else (append (list(first L2)) (merge (rest L2) L1))]
    ))

(define (subfirstlist L1 num)
  (cond
    [(= num 0) '()]
    [else (append (list (first L1)) (subfirstlist (rest L1) (- num 1)))]
    ))

(define (subseclist L1 num)
  (cond
    [(= num 0) '()]
    [(= num (length L1)) L1]
    [else (subseclist (rest L1) num)]
    ))

(define (firstlist L1)
  (subfirstlist L1 (floor (/ (length L1) 2))))

(define (secondlist L1)
  (subseclist L1 (ceiling (/ (length L1) 2))))

(define (mergesort L1)
  (cond
    [(null? (firstlist L1)) (secondlist L1)]
    [else (merge (mergesort (firstlist L1)) (mergesort (secondlist L1)))]
    ))

(provide (all-defined-out))