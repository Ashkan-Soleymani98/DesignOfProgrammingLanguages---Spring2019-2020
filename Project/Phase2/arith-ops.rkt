#lang racket

(require (except-in eopl #%module-begin))
(require racket/trace)

(define
  (is-all-num list)
  (cond
    [(null? list) #t]
    [(number? (first list)) (is-all-num (rest list))]
    [else #f]
    ))

(define
  (is-all-string list)
  (cond
    [(null? list) #t]
    [(string? (first list)) (is-all-string (rest list))]
    [else #f]
    ))

(define
  (is-all-bool list)
  (cond
    [(null? list) #t]
    [(boolean? (first list)) (is-all-bool (rest list))]
    [else #f]
    ))
    

(define
  (greater-list-num list num)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list contains elements other that numbers.")]
    [(null? list) #t]
    [(< (first list) num) #f]
    [(= (first list) num) #f]
    [else (greater-list-num (rest list) num)]
    ))

(define
  (greater-list-str list str)
  (cond
    [(boolean=? (is-all-string list) #f) (eopl:error "The list contains elements other that strings.")]
    [(null? list) #t]
    [(string<? (first list) str) #f]
    [(string=? (first list) str) #f]
    [else (greater-list-str (rest list) str)]
    ))

(define
  (greater-num-list num list)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list contains elements other that numbers.")]
    [(null? list) #t]
    [(< num (first list)) #f]
    [(= num (first list)) #f]
    [else (greater-num-list num (rest list))]
    ))

(define
  (greater-str-list str list)
  (cond
    [(boolean=? (is-all-string list) #f) (eopl:error "The list contains elements other that strings.")]
    [(null? list) #t]
    [(string<? str (first list)) #f]
    [(string=? str (first list)) #f]
    [else (greater-str-list str (rest list))]
    ))

(define
  (smaller-list-num list num)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list contains elements other that numbers.")]
    [(null? list) #t]
    [(> (first list) num) #f]
    [(= (first list) num) #f]
    [else (smaller-list-num (rest list) num)]
    ))

(define
  (smaller-list-str list str)
  (cond
    [(boolean=? (is-all-string list) #f) (eopl:error "The list contains elements other that strings.")]
    [(null? list) #t]
    [(string>? (first list) str) #f]
    [(string=? (first list) str) #f]
    [else (smaller-list-str (rest list) str)]
    ))

(define
  (smaller-num-list num list)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list contains elements other that numbers.")]
    [(null? list) #t]
    [(> num (first list)) #f]
    [(= num (first list)) #f]
    [else (smaller-num-list num (rest list))]
    ))

(define
  (smaller-str-list str list)
  (cond
    [(boolean=? (is-all-string list) #f) (eopl:error "The list contains elements other that strings.")]
    [(null? list) #t]
    [(string>? str (first list)) #f]
    [(string=? str (first list)) #f]
    [else (smaller-str-list str (rest list))]
    ))

(define
  (list-eq L1 L2)
  (cond
    [(and (null? L1) (null? L2)) #t]
    [(and (null? L1) (not (null? L2))) #f]
    [(and (null? L2) (not (null? L1))) #f]
    [(equal? (first L1) (first L2)) (list-eq (rest L1) (rest L2))]
    [else #f]
    ))

(define
  (list-eq-num L1 num)
  (cond
    [(boolean=? (is-all-num L1) #f) #f]
    [(null? L1) #t]
    [(= (first L1) num) (list-eq-num (rest L1) num)]
    [else #f]
    ))

(define
  (list-eq-str L1 str)
  (cond
    [(boolean=? (is-all-string L1) #f) #f]
    [(null? L1) #t]
    [(string=? (first L1) str) (list-eq-str (rest L1) str)]
    [else #f]
    ))

(define
  (list-eq-bool L1 bool)
  (cond
    [(boolean=? (is-all-bool L1) #f) #f]
    [(null? L1) #t]
    [(boolean=? (first L1) bool) (list-eq-bool (rest L1) bool)]
    [else #f]
    ))

(define
  (list-eq-null L1)
  (cond
    [(null? L1) #t]
    [(or (number? (first L1)) (string? (first L1)) (boolean? (first L1))) #f] 
    [(or (null? (first L1)) (symbol=? (first L1) 'null)) (list-eq-null (rest L1))]
    [else #f]
    ))

(define
  (list-rev L1)
  (cond
    [(null? L1) null]
    [(number? (first L1)) (append (list(* (first L1) -1)) (list-rev (rest L1)))]
    [(and (boolean? (first L1)) (boolean=? #t (first L1))) (append (list #f) (list-rev (rest L1)))]
    [(and (boolean? (first L1)) (boolean=? #f (first L1))) (append (list #t) (list-rev (rest L1)))]
    [(list? (first L1)) (append (list(list-rev (first L1))) (list-rev (rest L1)))]
    [else (eopl:error "The list contains elements that are not reversible.")]
    ))

(define (list-arith L1 operand arg)
    (cond
    [(boolean=? (is-all-num L1) #f) (eopl:error "The list contains elements other that numbers.")]
    [(null? L1) null]
    [else (append (list(arith-co (first L1) operand arg)) (list-arith (rest L1) operand arg))]
    ))

(define (list-arith-prime L1 operand arg)
    (cond
    [(boolean=? (is-all-num L1) #f) (eopl:error "The list contains elements other that numbers.")]
    [(null? L1) null]
    [else (append (list(arith-co arg operand (first L1))) (list-arith-prime (rest L1) operand arg))]
    ))

(define (list-bool L1 operand arg)
    (cond
    [(boolean=? (is-all-bool L1) #f) (eopl:error "The list contains elements other that booleans.")]
    [(null? L1) null]
    [else (append (list(arith-co (first L1) operand arg)) (list-bool (rest L1) operand arg))]
    ))

(define (list-string-first L1 str)
    (cond
    [(boolean=? (is-all-string L1) #f) (eopl:error "The list contains elements other that strings.")]
    [(null? L1) null]
    [else (append (list(string-append str (first L1))) (list-string-first (rest L1) str))]
    ))

(define (list-string-last L1 str)
    (cond
    [(boolean=? (is-all-string L1) #f) (eopl:error "The list contains elements other that strings.")]
    [(null? L1) null]
    [else (append (list(string-append (first L1) str)) (list-string-last (rest L1) str))]
    ))

(define (bool-or B1 B2)
  (cond
    [(and (boolean=? B1 #f) (boolean=? B2 #f)) #f]
    [else #t]
    ))

(define (bool-and B1 B2)
  (cond
    [(and (boolean=? B1 #t) (boolean=? B2 #t)) #t]
    [else #f]
    ))




(define
  (greater arg1 arg2)
  (cond
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #f]
    [(and (list? arg1) (number? arg2)) (greater-list-num arg1 arg2)]
    [(and (number? arg1) (list? arg2)) (greater-num-list arg1 arg2)]
    [(and (list? arg1) (string? arg2)) (greater-list-str arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (greater-str-list arg1 arg2)]
    [else ((eopl:error "Type Mismatch! Two arguments are not comparable!"))]
    ))

(define
  (smaller arg1 arg2)
  (cond
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #f]
    [(and (list? arg1) (number? arg2)) (smaller-list-num arg1 arg2)]
    [(and (number? arg1) (list? arg2)) (smaller-num-list arg1 arg2)]
    [(and (list? arg1) (string? arg2)) (smaller-list-str arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (smaller-str-list arg1 arg2)]
    [else ((eopl:error "Type Mismatch! Two arguments are not comparable!"))]
    ))

(define
  (equality arg1 arg2)
  ;(display arg1)
  ;(display arg2)
  (cond
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #f]
    [(and (null? arg1) (null? arg2)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #t) (boolean=? arg2 #t)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #t) (boolean=? arg2 #f)) #f]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #f) (boolean=? arg2 #f)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #f) (boolean=? arg2 #t)) #t]
    [(and (list? arg1) (list? arg2) (not (null? arg1)) (not (null? arg2))) (list-eq arg1 arg2)]
    [(and (list? arg1) (number? arg2)) (list-eq-num arg1 arg2)]
    [(and (number? arg1) (list? arg2)) (list-eq-num arg2 arg1)]
    [(and (list? arg1) (string? arg2)) (list-eq-str arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (list-eq-str arg2 arg1)]
    [(and (list? arg1) (boolean? arg2)) (list-eq-bool arg1 arg2)]
    [(and (boolean? arg1) (list? arg2)) (list-eq-bool arg2 arg1)]
    [(and (list? arg1) (null? arg2)) (list-eq-null arg1)]
    [(and (null? arg1) (list? arg2)) (list-eq-null arg2)]
    [else #f]
    ))

(define
  (inequality arg1 arg2)
  (cond
    [(boolean=? (equality arg1 arg2) #f) #t]
    [(boolean=? (equality arg1 arg2) #t) #f]
    ))

(define
  (converse arg1)
  (cond
    [(number? arg1) (* arg1 -1)]
    [(and (boolean? arg1) (boolean=? #t arg1)) #f]
    [(and (boolean? arg1) (boolean=? #f arg1)) #t]
    [(list? arg1) (list-rev arg1)]
    [else (eopl:error "The argument is not reversible")]
    ))

(define
  (arith-co arg1 arg2 arg3)
  (cond
    [(and (number? arg1) (number? arg3) (string=? arg2 "*")) (* arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "+")) (+ arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "-")) (- arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "/") (= arg3 0)) (eopl:error "Division By Zero")]
    [(and (number? arg1) (number? arg3) (string=? arg2 "/") (not(= arg3 0))) (/ arg1 arg3)]
    [(and (number? arg1) (list? arg3)) (list-arith-prime arg3 arg2 arg1)]
    [(and (list? arg1) (number? arg3)) (list-arith arg1 arg2 arg3)]
    [(and (boolean? arg1) (boolean? arg3) (string=? arg2 "*")) (bool-and arg1 arg3)]
    [(and (boolean? arg1) (boolean? arg3) (string=? arg2 "+")) (bool-or arg1 arg3)]
    [(and (boolean? arg1) (list? arg3)) (list-bool arg3 arg2 arg1)]
    [(and (list? arg1) (boolean? arg3)) (list-bool arg1 arg2 arg3)]
    [(and (string? arg1) (string? arg3) (string=? arg2 "+")) (string-append arg1 arg3)]
    [(and (list? arg1) (list? arg3) (string=? arg2 "+")) (append arg1 arg3)]
    [(and (string? arg1) (list? arg3) (string=? arg2 "+")) (list-string-first arg3 arg1)]
    [(and (list? arg1) (string? arg3) (string=? arg2 "+")) (list-string-last arg1 arg3)]
    [else (eopl:error "Operand is not applicable on these operators!")]
    ))

(provide (all-defined-out))


